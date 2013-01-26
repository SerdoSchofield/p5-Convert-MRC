package t::TestMRCConverter;
use Test::Base -Base;
use TBX::convert::MRC;

package t::TestMRCConverter::Filter;
use Test::Base::Filter -base;
use TBX::convert::MRC;
use Data::Dumper;
# use Data::Dumper;

my $converter = TBX::convert::MRC->new();
my $version = $TBX::convert::MRC::VERSION;

sub print_self {
	print Dumper $converter;
	return @_;
}

sub convert {
	my ($mrc) = @_;
	
	#read MRC from input string
	open my $mrc_handle, '<', \$mrc;
	
	#send output to strings
	my ( $tbx, $log );
	open my $tbx_handle, '>', \$tbx;
	open my $log_handle, '>', \$log;
	
	$converter->input_fh( $mrc_handle );
	$converter->tbx_fh($tbx_handle);
	$converter->log_fh($log_handle);
	$converter->convert;
	
	close $mrc_handle;
	close $tbx_handle;
	close $log_handle;
	
	#remove datetime stamps from log
	$log = remove_datetime(undef, $log);
	
	#return output TBX and log
	return [$tbx, $log];
}

sub no_tbx {
	my ($tbx_log) = @_;
	
	return [ undef, $tbx_log->[1] ];
}

sub remove_datetime {
	my ($text) = @_;
	defined $text 
		or return;
	$text =~ s/\] \[[^\]]+\]/\]/gm;
	return $text;
}

# fix version numbers and chomp logs
# so that they can be matched properly
sub fix_version {
	my ($log) = @_;
	
	chomp $log;
	#fix version
	$log =~ s/\[version\]/$TBX::convert::MRC::VERSION/;
	
	return $log;
}


#an argument like LHS,stateImpCond,attrValueTests,0 will return
# $root->{LHS}->{stateImpCond}->{attrValueTests}->[0]
# sub dive {
	# my ($root) = shift @_;
	# my @path = split ',', Test::Base::Filter::current_arguments();
	# use Data::Diver qw (Dive);
	# return Dive($root,@path);
# }


1;
