#test that the module is loaded properly

use strict;
use warnings;
use Test::More tests => 2;

use_ok( 'TBX::convert::MRC', 'use' );
is( ref( TBX::convert::MRC->new ) => 'TBX::convert::MRC', 'class' );

__END__
