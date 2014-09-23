(function() {
    var Index, TermQuery, BoolShouldQuery;
    function Index() {
        var inverted = {};
        var forward = [];
        this._ngram = function(s,min,max,fx) {
            var chars = s.split('');
            for (var pos = 0; pos < chars.length; pos++) {
                var current = '';
                for (var n = min; n <= max; n++) {
                    for (var i = current.length; i < n && i + pos < chars.length; i++) {
                        current += chars[i + pos];
                    }
                    if (current.length === n) {
                        fx(current);
                    }
                }
            }
        };

        this._tokenize = function(s,fx) {
            var splitted = s.toLowerCase().split(/[^\w]+/)
            for (var i = 0; i < splitted.length; i++) {
                if (splitted[i].length > 0)
                    fx(splitted[i]);
            }
        };

        this.analyzer = function(s,fx) {
            var self = this;
            self._tokenize(s,function(token) {
                self._ngram(token,2,3,function(gram) {
                    fx(gram);
                })
            });
        };
        this.push_inverted = function(token, doc_id, payload) {
            if (!(token in inverted))
                inverted[token] = []
            inverted[token].push((doc_id << 16) | (payload & 0xFFFF))
        };
        this.push_forward = function(id) {
            var doc_id = forward.length;
            forward.push(id);
            return doc_id;
        };

        this.bulk_index = function(id,texts) {
            var uniq = {};
            for (var k = 0; k < texts.length; k++) {
                this.analyzer(texts[k],function(gram) {
                    var v = 0;
                    if (gram in uniq)
                        v = uniq[gram];
                    if (k == 0)
                        v++; // count tf only in the first text
                    uniq[gram] = v;
                });
            }

            var doc_id = this.push_forward(id);
            for (var gram in uniq) {
                this.push_inverted(gram,doc_id,(uniq[gram] & 0xFFFF));
            }
        };

        this.term_query = function(token) {
            if (token in inverted) {
                return new TermQuery(inverted[token]);
            }
            return undefined;
        };

        this.match_query = function (s){
            var query = new BoolShouldQuery;
            var self = this;
            this.analyzer(s,function(token) {
                query.add(self.term_query(token));
            })
            if (query.length == 0)
                return undefined;
            return query;
        };
        this.search = function(query,fx,limit) {
            var scored = [];
            var n_docs = forward.length.toFixed(2);
            query.collect(function (query) {
                var doc_id = query.doc_id;
                var tf = (doc_id & 0xFFFF);
                doc_id >>= 16;
                if (tf == 0)
                    tf = 1;
                // tf * idf
                return tf * (1 + Math.log(n_docs / (query.count() + 1)));
            },function(doc,score) {
                scored.push([doc,score]);
            });
            // fixme: use priority queue
            scored.sort(function(a,b) { b[1] - a[1] })
            for (var i = 0; i < scored.length && limit > 0; i++, limit--) {
                fx(forward[scored[i][0]],scored[i][1]);
            }
        };
    };

    function BoolShouldQuery() {
        this.prototype = new Array;
        this.push = Array.prototype.push;
        this.doc_id = Number.MAX_VALUE;
        this.add = function(query) {
            if (query)
                this.push(query);
            return this;
        };
        this.collect = function(scorer,fx) {
            while(this.next() != Number.MAX_VALUE) {
                var score = 0;
                for (var i = 0; i < this.length; i++) {
                    if (this[i].doc_id == this.doc_id) {
                        score += scorer(this[i]);
                    }
                }
                fx(this.doc_id >> 16,score);
            }
        };

        this.next = function() {
            var new_doc = Number.MAX_VALUE;
            for (var i = 0; i < this.length; i++) {
                var cur_doc = this[i].doc_id;
                if (cur_doc === this.doc_id) cur_doc = this[i].next();
                if (cur_doc < new_doc) new_doc = cur_doc;
            }
            return this.doc_id = new_doc;
        };
    };

    function TermQuery (term_enum) {
        this.doc_id = Number.MAX_VALUE;

        var cursor = 0;
        var term_enum = term_enum;
        this.count = function() {
            return term_enum.length;
        };
        this.next = function() {
            if (cursor > term_enum.length - 1)
                return this.doc_id = Number.MAX_VALUE;
            if (this.doc_id != Number.MAX_VALUE)
                cursor++;
            
            return this.doc_id = term_enum[cursor];
        };
        this.collect = function(scorer,fx) {
            while(this.next() != Number.MAX_VALUE) {
                fx(this.doc_id >> 16, scorer(this));
            }
        };
    };
    window.Index = Index;
    window.BoolShouldQuery = BoolShouldQuery;
    window.TermQuery = TermQuery
}).call(this);
