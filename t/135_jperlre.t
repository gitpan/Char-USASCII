# This file is encoded in US-ASCII.
die "This file is not encoded in US-ASCII.\n" if q{あ} ne "\x82\xa0";

use Char::USASCII;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('ああいうえ' =~ /(あいう)$/) {
    print "not ok - 1 $^X $__FILE__ not ('ああいうえ' =~ /あいう\$/).\n";
}
else {
    print "ok - 1 $^X $__FILE__ not ('ああいうえ' =~ /あいう\$/).\n";
}

__END__
