# This file is encoded in US-ASCII.
die "This file is not encoded in US-ASCII.\n" if q{��} ne "\x82\xa0";

use Char::USASCII;
print "1..2\n";

my $__FILE__ = __FILE__;

open(FILE,">$__FILE__.txt") || die;
print FILE <DATA>;
close(FILE);

open(GETC,"<$__FILE__.txt") || die;
my @getc = ();
while (my $c = Char::USASCII::getc(GETC)) {
    last if $c eq "\n";
    push @getc, $c;
}
close(GETC);

my $result = join('', map {"($_)"} @getc);
if ($result eq '(1)(2)(�)(�)') {
    print "ok - 1 $^X $__FILE__ 12�� --> $result.\n";
}
else {
    print "not ok - 1 $^X $__FILE__ 12�� --> $result.\n";
}

{
    package Getc::Test;

    open(GETC2,"<$__FILE__.txt") || die;
    my @getc = ();
    while (my $c = Char::USASCII::getc(GETC2)) {
        last if $c eq "\n";
        push @getc, $c;
    }
    close(GETC2);

    my $result = join('', map {"($_)"} @getc);
    if ($result eq '(1)(2)(�)(�)') {
        print "ok - 2 $^X $__FILE__ 12�� --> $result.\n";
    }
    else {
        print "not ok - 2 $^X $__FILE__ 12�� --> $result.\n";
    }
}

unlink("$__FILE__.txt");

__END__
12��
