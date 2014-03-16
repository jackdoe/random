#!env perl
use strict;
use warnings;
use HTTP::Tiny;
use JSON qw(decode_json);
use Getopt::Long;
use Data::Dumper;
# most of it was hijacked from https://github.com/eyenx/yify.py

my %ALLOW = (
    genre => {
        "action"      => 1,
        "adventure"   => 1,
        "animation"   => 1,
        "biography"   => 1,
        "comedy"      => 1,
        "crime"       => 1,
        "documentary" => 1,
        "drama"       => 1,
        "family"      => 1,
        "fantasy"     => 1,
        "film-noir"   => 1,
        "history"     => 1,
        "horror"      => 1,
        "music"       => 1,
        "musical"     => 1,
        "mystery"     => 1,
        "romance"     => 1,
        "sci-fi"      => 1,
        "short"       => 1,
        "sport"       => 1,
        "thriller"    => 1,
        "war"         => 1,
        "western"     => 1,
        "ALL"         => 1
    },
    quality => {
        "720p"  => 1,
        "1080p" => 1,
        "3D"    => 1,
        "ALL"   => 1
    },
    sort => {
        "date"       => 1, 
        "seeds"      => 1,
        "peers"      => 1,
        "size"       => 1,
        "alphabet"   => 1,
        "rating"     => 1,
        "downloaded" => 1
    },
    order => {
        "desc" => 1,
        "asc"  => 1
    },
    rating => [ 0, 9 ],
    limit  => [ 1, 50 ],
    );

my %PARAMS = (
    sort    => 'seeds',
    order    => 'desc',
    genre    => 'ALL',
    quality  => 'ALL',
    limit    => 10,
    rating   => 0,
    keywords => ''
);

process_options(\%ALLOW,\%PARAMS);
my $request = "http://yify-torrents.com/api/list.json?" . join("&",map { $_ . "=" . $PARAMS{$_} } keys(%PARAMS));
print "GENERATED: $request\n";

my $response = HTTP::Tiny->new->get($request);
die "failed: $response->{reason}" 
    unless $response->{success};

my $json = decode_json($response->{content});
die "failed to parse '$response->{content}'"
    unless ref($json) eq 'HASH' && ref($json->{MovieList}) eq 'ARRAY';

my $red="\033[1;31m";
my $blue="\033[1;34m";
my $norm="\033[0m";

my $longest = 100;
my $flix = sub { "peerflix '$_[0]->{TorrentUrl}' -m #"; };
for my $row(@{ $json->{MovieList} }) {
    my $l = length($flix->($row));
    $longest = $l
        if $l > $longest;
}

for my $row(@{ $json->{MovieList} }) {
    printf "%".$longest."s %s%s%s [ %s %s %s%ss%s/%sp ]\n",
                  $flix->($row),
                  $blue,$row->{MovieTitleClean},$norm,
                  $row->{Size},
                  $row->{Quality},
                  $red,$row->{TorrentSeeds},$norm,
                  $row->{Downloaded}
}

# as simple as that.

sub process_options {
    my ($allow,$dest) = @_;
    my $parse = sub {
        my ($opt,$val) = @_;
        my $allowed = $allow->{$opt};
        die "'$val' is not valid option for $opt, use '$0 --help' to see all options"
            unless $allowed;

        die "'$val' is out of range for '$opt'"
            if (ref($allowed) eq 'ARRAY' && ($val < $allowed->[0] || $val > $allowed->[1]));
        $dest->{$opt} = $val;
    };

    my $keywords = "";
    GetOptions ("genre=s"   => $parse,
                "quality=s" => $parse, 
                "sort=s"    => $parse,
                "order=s"   => $parse,
                "limit=i"   => $parse,
                "rating=i"  => $parse,
                "search=s"  => \$keywords,
                "<>"        => sub { $keywords .= $_[0] . " " },
                "help"      => sub { print "$0 search string\n\nhere is a list of all options: \n" . Dumper($allow); exit 1; }
        );
    $dest->{keywords} = $keywords;
}
