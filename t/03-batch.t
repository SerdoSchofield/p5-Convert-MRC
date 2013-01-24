#test that the original example MRC converts correctly

use t::TestMRCConverter;
use FindBin qw($Bin);
use File::Spec::Functions;
use Test::More 0.96;
plan tests => 2 * blocks;
use Test::LongString;
use Test::XML;
use File::Slurp;
use File::Touch;

#convert two files in the data directory
my $data_dir = catdir( $Bin, 'data');

my @batch_files;

#the operations of this loop ensure that this test file can be run multiple times and still work
for my $block ( blocks ){

	#make sure that the log and tbx files you expect to create
	#don't already exist; batch() won't overwrite existing files
	my $file = catfile($data_dir, $block->log_name);
	unlink $file if -e $file;
	$file = catfile($data_dir, $block->tbx_name);
	unlink $file if -e $file;
	
	# create files as specified; 
	# this is used to get the batcher to find another file name (it never overwrites existing files)
	if($block->create_file){
		for($block->create_file){
			$file = catfile($data_dir, $_);
			if(not -e $file){
				touch $file;
			}
		}
	}
	
	#input is name of the file to read in batch processing
	push @batch_files, catfile($data_dir, $block->input);
}

#run MRC files to TBX
my $converter = TBX::convert::MRC->new;
$converter->batch(@batch_files);

#check existence and content of output files
for my $block ( blocks ){
	
	subtest 
		'log file for ' . $block->input . ' written correctly' => sub {
		
		plan tests => 2;
	
		my $log_file_name = catfile($data_dir, $block->log_name);
		ok(-e $log_file_name, $block->log_name . ' was created')
			or return;
		
		my $log = read_file($log_file_name);
		$log = t::TestMRCConverter::Filter::remove_datetime(undef,$log);
		is_string(
			$log, $block->log_contents,  
			"'" . $block->name . "' correct message(s) logged")
			or print $log;
	};
	
	subtest 
		'TBX file for ' . $block->input . ' written correctly' => sub {
		
		plan tests => 2;
	
		my $tbx_file_name = catfile($data_dir, $block->tbx_name);
		ok(-e $tbx_file_name, $block->tbx_name . ' was created')
			or return;
		
		my $tbx = read_file($tbx_file_name);\
		is_xml(
			$tbx, $block->tbx_contents, 
			"'" . $block->name . "' correct TBX output");
	};
}

__DATA__
=== no renaming required

--- input: chicken.txt

--- log_name: chicken.txt.log

--- log_contents fix_version
[MSG] MRC2TBX converter version [version]
[MSG] File includes IDs:
	C003
	C003fr1
	C003en1

--- tbx_name: chicken.txt.tbx

--- tbx_contents fix_version
<?xml version='1.0' encoding="UTF-8"?>
<!DOCTYPE martif SYSTEM "TBXBasiccoreStructV02.dtd">
<martif type="TBX-Basic-V1" xml:lang="en">
	<martifHeader>
		<fileDesc>
			<titleStmt>
				<title>termbase from MRC file</title>
			</titleStmt>
			<sourceDesc>
				<p>generated by TBX::convert::MRC version [version]</p>
			</sourceDesc>
			<sourceDesc>
				<p>a restaurant menu in English and French</p>
			</sourceDesc>
		</fileDesc>
		<encodingDesc>
			<p type="DCSName">TBXBasicXCSV02.xcs</p>
		</encodingDesc>
	</martifHeader>
	<text>
		<body>
			<termEntry id="C003">
				<descripGrp>
					<descrip type="subjectField">Restaurant Menus</descrip>
				</descripGrp>
				<langSet xml:lang="fr">
					<tig id="C003fr1">
						<term>poulet</term>
						<termNote type="partOfSpeech">noun</termNote>
						<termNote type="grammaticalGender">masculine</termNote>
					</tig>
				</langSet>
				<langSet xml:lang="en">
					<tig id="C003en1">
						<term>chicken</term>
						<termNote type="partOfSpeech">noun</termNote>
					</tig>
				</langSet>
			</termEntry>
		</body>
	</text>
</martif>

=== some renaming required because of existing log

--- input: garbanzo.txt

--- create_file lines chomp
garbanzo.txt.log

--- log_name: garbanzo.txt-1.log

--- log_contents fix_version
[MSG] MRC2TBX converter version [version]
[MSG] File includes links to:
	http://flickr.com/photos/lilgreen/432468210/
[MSG] File includes IDs:
	C005
	C005en1
	C005en2
	C005fr1

--- tbx_name: garbanzo.txt-1.tbx

--- tbx_contents fix_version
<?xml version='1.0' encoding="UTF-8"?>
<!DOCTYPE martif SYSTEM "TBXBasiccoreStructV02.dtd">
<martif type="TBX-Basic-V1" xml:lang="en">
	<martifHeader>
		<fileDesc>
			<titleStmt>
				<title>termbase from MRC file</title>
			</titleStmt>
			<sourceDesc>
				<p>generated by TBX::convert::MRC version [version]</p>
			</sourceDesc>
			<sourceDesc>
				<p>a restaurant menu in English and French</p>
			</sourceDesc>
		</fileDesc>
		<encodingDesc>
			<p type="DCSName">TBXBasicXCSV02.xcs</p>
		</encodingDesc>
	</martifHeader>
	<text>
		<body>
			<termEntry id="C005">
				<descripGrp>
					<descrip type="subjectField">Restaurant Menus</descrip>
				</descripGrp>
				<xref type="xGraphic" target="http://flickr.com/photos/lilgreen/432468210/">garbanzo beans</xref>
				<langSet xml:lang="en">
					<descripGrp>
						<descrip type="definition">an edible legume of the family Fabaceae, subfamily Faboideae</descrip>
						<admin type="source">http://en.wikipedia.org/wiki/Chickpea</admin>
					</descripGrp>
					<tig id="C005en1">
						<term>chick peas</term>
						<termNote type="partOfSpeech">noun</termNote>
					</tig>
					<tig id="C005en2">
						<term>garbanzo beans</term>
						<termNote type="partOfSpeech">noun</termNote>
						<termNote type="geographicalUsage">southwest United States</termNote>
						<admin type="customerSubset">AlmostRipe Foods</admin>
					</tig>
				</langSet>
				<langSet xml:lang="fr">
					<tig id="C005fr1">
						<term>pois chiches</term>
						<termNote type="partOfSpeech">noun</termNote>
					</tig>
				</langSet>
			</termEntry>
		</body>
	</text>
</martif>

=== some renaming required because of existing TBX

--- input: chair.txt

--- create_file lines chomp
chair.txt.tbx
chair.txt-1.tbx

--- log_name: chair.txt-2.log

--- log_contents fix_version
[MSG] MRC2TBX converter version 3.4
[MSG] File includes IDs:
	C005
	C005en1
	C005en2

--- tbx_name: chair.txt-2.tbx

--- tbx_contents fix_version
<?xml version='1.0' encoding="UTF-8"?>
<!DOCTYPE martif SYSTEM "TBXBasiccoreStructV02.dtd">
<martif type="TBX-Basic-V1" xml:lang="en">
	<martifHeader>
		<fileDesc>
			<titleStmt>
				<title>termbase from MRC file</title>
			</titleStmt>
			<sourceDesc>
				<p>generated by TBX::convert::MRC version 3.4</p>
			</sourceDesc>
			<sourceDesc>
				<p>wood shop manual</p>
			</sourceDesc>
		</fileDesc>
		<encodingDesc>
			<p type="DCSName">TBXBasicXCSV02.xcs</p>
		</encodingDesc>
	</martifHeader>
	<text>
		<body>
			<termEntry id="C005">
				<descripGrp>
					<descrip type="subjectField">woodworking</descrip>
				</descripGrp>
				<langSet xml:lang="en">
					<tig id="C005en1">
						<term>chair</term>
						<termNote type="partOfSpeech">noun</termNote>
					</tig>
					<tig id="C005en2">
						<term>seat</term>
						<termNote type="partOfSpeech">noun</termNote>
					</tig>
				</langSet>
			</termEntry>
		</body>
	</text>
</martif>


