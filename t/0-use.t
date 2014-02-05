#test that the module is loaded properly

use strict;
use warnings;
use Test::More tests => 1;

use Convert::MRC;
is( ref( Convert::MRC->new ) => 'Convert::MRC', 'class' );

__END__
