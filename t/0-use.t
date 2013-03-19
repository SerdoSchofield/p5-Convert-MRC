#test that the module is loaded properly

use strict;
use warnings;
use Test::More tests => 2;

use_ok( 'MRC::Convert', 'use' );
is( ref( MRC::Convert->new ) => 'MRC::Convert', 'class' );

__END__
