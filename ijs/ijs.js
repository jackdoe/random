var Index = function() {
    var inverted = {};
    var forward = [];
    var tokenizer_re = new RegExp('[ \n\r?!.,_-]+');
    var self = this;

    this.count = function() {
        return forward.length;
    };

    this.ngram = function(s,min,max,fx) {
        var chars = s.split('');
        for (var pos = 0; pos < chars.length; pos++) {
            var current = '';
            for (var n = min; n <= max; n++) {
                for (var i = current.length; i < n && i + pos < chars.length; i++) {
                    current += chars[i + pos];
                }

                if (current.length === n)
                    fx(current);
            }
        }
    };

    this.tokenize = function(s,fx) {
        var splitted = s.toLowerCase().split(tokenizer_re)
        for (var i = 0; i < splitted.length; i++) {
            if (splitted[i].length > 0)
                fx(splitted[i]);
        }
    };

    this.analyzer = function(s,fx) {
        self.tokenize(s,function(token) {
            self.ngram(token,2,3,function(gram) {
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
                if (k === 0)
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
            return new TermQuery(this,inverted[token],token);
        }
        return undefined;
    };

    this.match_query = function (s){
        var query = new BoolShouldQuery;
        this.analyzer(s,function(token) {
            query.add(self.term_query(token));
        })
        return query;
    };

    this.collect = function(query,scorer,fx) {
        while(query.next() !== Number.MAX_VALUE) {
            var score = query.score(scorer);
            if (score > 0)
                fx(query.doc_id,score);
        }
    };

    this.search = function(query,fx,limit) {
        var scored = [];
        this.collect(query,function (term) {
            var tf = term.tf;
            if (tf === 0)
                tf = 1;
            return tf * term.idf;
        },function(doc,score) {
            scored.push([doc,score]);
        });

        // fixme: use priority queue
        scored.sort(function(a,b) { return b[1] - a[1] })
        for (var i = 0; i < scored.length && limit > 0; i++, limit--) {
            fx(forward[scored[i][0]],scored[i][1]);
        }
    };

};

var BoolShouldQuery = function (){
    this.prototype = new Array;
    this.push = Array.prototype.push;
    this.doc_id = -1;
    this.add = function(query) {
        if (query)
            this.push(query);
        return this;
    };

    this.score = function(scorer) {
        var score = 0;
        for (var i = 0; i < this.length; i++) {
            if (this[i].doc_id === this.doc_id) {
                score += this[i].score(scorer);
            }
        }
        return score;
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

    this.jump = function(target) {
        var new_doc = Number.MAX_VALUE;
        for (var i = 0; i < this.length; i++) {
            var cur_doc = this[i].doc_id;
            if (cur_doc < target) cur_doc = this[i].jump(target);
            if (cur_doc < new_doc) new_doc = cur_doc;
        }

        return this.doc_id = new_doc;
    };

    this.count = function() {
        var c = 0;
        for (var i = 0; i < this.length; i++)
            c += this[i].count();
        return c;
    };
};


var BoolMustQuery = function (){
    this.prototype = new Array;
    this.push = Array.prototype.push;
    this.sort = Array.prototype.sort;
    this.doc_id = -1;
    var lead = undefined;

    this.add = function(query) {
        if (query) {
            this.push(query);
            this.sort(function(a,b) { return a.count() - b.count() });
            lead = this[0];
        } else {

            this.length = 0;
        }
        return this;
    };

    this.__jump = function(target) {
        if (lead === undefined || this.length == 0)
            return this.doc_id = Number.MAX_VALUE;

        for (var i = 1; i < this.length; i++) {
            var n = this[i].jump(target);
            if (n > target) {
                target = lead.jump(n);
                i = 1;
            }
        }

        return this.doc_id = lead.doc_id;
    };

    this.next = function() {
        return this.__jump(lead.next());
    };

    this.jump = function(target) {
        return this.__jump(lead.jump(target));
    };

    this.count = function() {
        for (var i = 0; i < this.length; i++)
            return this[i].count();
        return 0;
    };

    this.score = function(scorer) {
        var score = 0;
        for (var i = 0; i < this.length; i++) {
            score += this[i].score(scorer);
        }
        return score;
    };
};

var TermQuery = function(index,term_enum,token) {
    this.doc_id = -1;
    this.idf = (1 + Math.log(index.count() / (term_enum.length + 1)));
    this.tf = 0;
    this.token = token;
    var cursor = 0;
    var term_enum = term_enum;

    this.update = function() {
        if (cursor > term_enum.length - 1)
            return this.doc_id = Number.MAX_VALUE;

        var doc_id = term_enum[cursor];
        this.tf = doc_id & 0xFFFF;
        return this.doc_id = doc_id >> 16;
    };

    this.count = function() {
        return term_enum.length;
    };

    this.next = function() {
        if (this.doc_id !== -1)
            cursor++;

        return this.update();
    };

    this.jump = function(target) {
        if (cursor > term_enum.length - 1)
            return this.doc_id = Number.MAX_VALUE;

        if (this.doc_id === target || target === Number.MAX_VALUE)
            return this.doc_id = target;

        var end = this.count();
        var start = Math.min(0,cursor);
        while (start < end) {
            var mid = start + Math.floor((end - start) / 2)
            var doc = term_enum[mid] >> 16;
            if (doc == target) {
                start = mid;
                break;
            }
            if (doc < target)
                start = mid + 1;
            else
                end = mid;
        }
        cursor = start;
        return this.update();
    };

    this.score = function(scorer) {
        return scorer(this);
    }
};
