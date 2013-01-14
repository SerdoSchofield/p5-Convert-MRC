# MRC to TBX converter
# written June-Nov 2008 by Nathan E. Rasmussen

# The MRC format is fully described in an article by Alan K. Melby, 
# to appear in Tradumatica. At an approximation, it is a file of tab-
# separated rows, each consisting of an ID, a data category, and a value
# to be stored for that category in the object with the given ID. The file
# should be sorted on its first column. If it is not, the converter may 
# skip rows (if they are at too high a level) or end processing early
# (if the order of A-rows, C-rows, and R-rows is broken).

# This translator receives a file or list of files in this format and 
# emits TBX-Basic, a standard format for terminology interchange. 
# Incorrect or unusable input is skipped, with one exception, and the 
# problem is noted in a log file. The outputs generally have the same 
# filename as the inputs, and a suffix of .tbx and .warnings, but a number
# may be added to the filename to ensure the output filenames are unique. 

# The exception noted is this: If the user documents a party responsible
# for some change in the termbase, but does not state whether that party
# is a person or an organization, the party will be included in the TBX
# as a "respParty". This designation does not conform to the TBX-Basic 
# standard and will need to be changed (to "respPerson" or "respOrg")
# before the file will validate. This is one of the circumstances in which
# the converter will output invalid TBX-Basic. 

# The other circumstance is that a file might not contain a definition,
# a part of speech, or a context sentence for some term, or might not
# contain a term itself. The converter detects these and warns about them,
# but there is no way it could fix them. It does not detect or warn about
# concepts containing no langSet or langSets containing no term, but these
# are also invalid. 

# Example input data follows: 

# TEST DATA HERE





# ABSTRACT: CONVERT MRC TO TBX-BASIC
package TBX::convert::MRC;
use warnings;

use Log::Message::Simple qw (:STD);

our $VERSION = 3.4;
# VERSION

use open ':encoding(utf8)', ':std'; # incoming/outgoing data will be UTF-8

# subroutine declarations
sub parseRow;
sub buildHeader;
sub printHeader;
sub closeTerm; # do nothing if no term level is open
sub closeLangSet; # nothing if no lang level is open
sub closeConcept; # nothing if no concept level is open
sub sortRefs; # structure a term's worth of data rows for printing
sub printRow;

# reference variables


# How does the data category from a header row relate to the header?
# (This is also a validity check.)
our %corresp = (
	workingLanguage => 'Language',
	sourceDesc => 'Source',
	subjectField => 'Subject',
);

our $langCode = qr/[a-zA-Z]{2,3}(?:-[a-zA-Z]{2})?/;
	# ISO 639 language code, and optionally region code: fr, eng-US 
	# case-insensitive; values are smashed to lowercase when parsed

# What is the proper capitalization for each data category/picklist item?
# A hash from a case-smashed version to the correct version, which will be
# used to recognize and fix user input.
our %correctCaps;
$correctCaps{'DatCat'}{lc($_)} = $_ foreach qw (
	sourceDesc workingLanguage subjectField xGraphic definition 
	term partOfSpeech administrativeStatus context geographicalUsage
	grammaticalGender termLocation termType note source
	crossReference externalCrossReference customerSubset projectSubset
	transactionType fn org title role email uid tel adr type
);
$correctCaps{'partOfSpeech'}{lc($_)} = $_ foreach qw (
	noun verb adjective adverb properNoun other
);
$correctCaps{'administrativeStatus'}{lc($_)} = $_ foreach qw (
	preferredTerm-admn-sts admittedTerm-admn-sts
	deprecatedTerm-admn-sts supersededTerm-admn-sts
);
$correctCaps{'grammaticalGender'}{lc($_)} = $_ foreach qw (
	masculine feminine neuter other
);
$correctCaps{'termLocation'}{lc($_)} = $_ foreach qw (
	menuItem dialogBox groupBox textBox comboBox comboBoxElement
	checkBox tab pushButton radioButton spinBox progressBar slider
	informativeMessage interactiveMessage toolTip tableText
	userDefinedType
);
$correctCaps{'termType'}{lc($_)} = $_ foreach qw (
	fullForm acronym abbreviation shortForm variant phrase
);
$correctCaps{'transactionType'}{lc($_)} = $_ foreach qw (
	origination modification
);
$correctCaps{'type'}{lc($_)} = $_ foreach qw (
	person organization
);

# Which additional fields are allowed on which data categories?
our %allowed; 
$allowed{$_}{'Note'} = 1 foreach qw ();
$allowed{$_}{'Source'} = 1 foreach qw (
	definition subjectField context
);
$allowed{$_}{'Link'} = 1 foreach qw (
	transactionType crossReference externalCrossReference xGraphic
);
$allowed{'transactionType'}{'Date'} = 
	$allowed{'transactionType'}{'Responsibility'} =
	1;
$allowed{$_}{'FieldLang'} = 1 foreach qw (Source Note Responsibility);

# which data categories are allowed at which level?
our %legalIn;
$legalIn{'Concept'}{$_} = 1 foreach qw (transactionType crossReference externalCrossReference customerSubset projectSubset xGraphic subjectField note source);
$legalIn{'LangSet'}{$_} = 1 foreach qw (transactionType crossReference externalCrossReference customerSubset projectSubset definition note source);
$legalIn{'Term'}{$_} = 1 foreach qw (transactionType crossReference externalCrossReference customerSubset projectSubset context grammaticalGender geographicalUsage partOfSpeech termLocation termType administrativeStatus note source term);
$legalIn{'Party'}{$_} = 1 foreach qw (email title role org uid tel adr fn);

# what part of the term structure does each data category go in?
our %position;
%position = map { $_ => 'termGrp' } qw (administrativeStatus geographicalUsage grammaticalGender partOfSpeech termLocation termType);
%position = (%position, map { $_ => 'auxInfo' } qw (context customerSubset projectSubset crossReference note source transactionType externalCrossReference xGraphic));

# which TBX meta data category does each data category print as?
our %meta;
$meta{$_} = 'admin' foreach qw (customerSubset projectSubset);
$meta{$_} = 'descrip' foreach qw (definition subjectField context);
$meta{$_} = 'termNote' foreach qw (grammaticalGender geographicalUsage partOfSpeech termLocation termType administrativeStatus);

# global status flags

# main code

our @origARGV = @ARGV;
@ARGV = ('-') unless @ARGV; # if no filenames given, take std input
#use batch() if called as a script
__PACKAGE__->new->batch(@ARGV) unless caller;

=head1 METHODS

=head2 C<new>

Creates and returns a new instance of TBX::convert::MRC.

=cut

sub new {
  my ($class) = @_;
  my $self = bless {}, $class;
  # $self->_init;
  $self;
}

=head2 C<tbx_fh>

Optional argument: string file path or GLOB

Sets and/or returns the file handle used to print the converted TBX.

=cut

sub tbx_fh {
	my ( $application, $fh ) = @_;
	if ($fh) {
		if(ref($fh) eq 'GLOB'){
			$application->{tbx_fh} = $fh;
		}
		else{
			open my $fh2, '>', $fh or die "Couldn't open $fh";
			$application->{tbx_fh} = $fh2;
		}
	}
	$application->{tbx_fh};
}

=head2 C<log_fh>

Optional argument: string file path or GLOB

Sets and/or returns the file handle used to log any messages.

=cut

sub log_fh {
	my ( $application, $fh ) = @_;
	if ($fh) {
		if(ref($fh) eq 'GLOB'){
			$application->{log_fh} = $fh;
		}
		else{
			open my $fh2, '>', $fh or die "Couldn't open $fh";
			$application->{log_fh} = $fh2;
		}
	}
	$application->{log_fh};
}

=head2 C<log>

Argument: message to log

Prints the given message to the log filehandle.
=cut

sub log {
	my ($self, $message) = @_;
	print {$self->{log_fh}} $message;
}

=head2 C<input_fh>

Optional argument: string file path or GLOB

Sets and/or returns the file handle used to read the MRC data from.

=cut
sub input_fh {
	my ( $application, $fh ) = @_;
	if ($fh) {
		if(ref($fh) eq 'GLOB'){
			$application->{input_fh} = $fh;
		}
		else{
			open my $fh2, '<', $fh or die "Couldn't open $fh";
			$application->{input_fh} = $fh2;
		}
	}
	$application->{input_fh};
}

=head2 C<batch>
Processes each of the input files, printing the converted TBX file to a file with the same name and the suffix ".tbx".
Warnings are also printed to a file with the same name and the suffix ".warnings".

=cut

sub batch {
	my ($self, @mrc_files) = @_;
	for my $mrc (@mrc_files) {
		# find an appropriate name for output and warning files
		my $suffix = _get_suffix($mrc);

		#set output, error and input files
		my $outTBX = "$mrc$suffix.tbx";
		my $outWarn = "$mrc$suffix.log";
		print STDERR "See $outTBX and $outWarn for output.\n";
		$self->input_fh($mrc);
		$self->log_fh($outWarn);
		$self->tbx_fh($outTBX);
		
		#convert the input file, sending output to appropriate file handles
		$self->convert;
		print STDERR "Finished processing $mrc.\n";
	}
}

sub convert {
	my ($self) = @_;
	my $select = select $self->{tbx_fh};

	# informative header for the log file
	msg( "MRC2TBX converter version $VERSION");
	msg("Called with " . scalar @origARGV . " argument" . ( @origARGV == 1 ? '' : 's' ) . 
		":\n\t" . (join "\t", @origARGV));

	# set up per-file status flags
	my %header; # contains the header information
	my $segment = 'header'; # header, body, back
	local ($concept, $langSet, $term, $party); # what's open
		# locals are accessible to subroutines
	local $def = 0; # whether current langSet has a definition
	local @unsortedTerm; # collect all rows for an ID
	my @party; # collect all rows for a responsible party
	my %responsible; # accumulate parties by type
	my (@idsUsed, @linksMade); # track these 
	my $started = 0; # flag for MRCTermTable line (start-of-processing)
	my $aborted = 0; # flag for early end-of-processing

	# process the file
	while (readline($self->{input_fh})) {
		# eliminate a (totally superfluous) byte-order mark
		s/^(?:\xef\xbb\xbf|\x{feff})// if $. == 1;
		next unless /^=MRCtermTable/i .. 0; # start processing
		$started = 1;
		next if /^=MRCtermTable/i; # but let that one go
		next if (/^\s*$/); # if it's only whitespace
		my $row;
		next unless $row = parseRow($_);
		# (if the row won't parse, parseRow() returns undef)

		# if in header, A row?

		# A-row: build header
		if ($segment eq 'header' && $row->{'ID'} eq 'A') {
			buildHeader( parseRow($_), \%header) 
				or error "Could not interpret header line $., skipped.";
		}

		# not A-row: print header, segment = body
		if ($segment eq 'header' && $row->{'ID'} ne 'A') {
			# better have enough for a header! 
			unless (printHeader(\%header)) {
				error "TBX header could not be completed because a required A-row is missing or malformed.";
				$aborted = 1; 
				last;
			}
			$segment = 'body';
		}

		# if in body, C row?

		# C-row: lots to do
		if ($segment eq 'body' && exists $row->{'Concept'}) {
			# catch a misordered-input problem

# The next 3 if tests are one action in principle.
# Each depends on the preceding, and all depend on the
# closeX() subs being no-ops if it's already closed,
# and on the fact that nothing follows terms in langSet
# or follows langSet in termEntry. Meddle not, blah blah.

			{ no warnings 'uninitialized'; 
			# $concept, $langSet, $term might be undef
			# if new concept, close old and open new
			if ($row->{'Concept'} ne $concept) {
				closeTerm();
				closeLangSet();
				closeConcept();
				# open concept
				$concept = $row->{'Concept'};
				print "<termEntry id=\"C$concept\">\n";
				# (not row ID, which may go further)
				push @idsUsed, "C$concept";
			}
			# if new langSet ...
			if (exists $row->{'LangSet'} && 
				$row->{'LangSet'} ne $langSet) {
				closeTerm();
				closeLangSet();
				
				# open langSet
				$langSet = $row->{'LangSet'};
				print "<langSet xml:lang=\"$langSet\">\n";
			}
			# if new term ...
			if (exists $row->{'Term'} && 
				$row->{'Term'} ne $term) {
				closeTerm();
				# open term
				$term = $row->{'Term'};
				undef @unsortedTerm; # redundant
				push @idsUsed, "C$concept$langSet$term";
			}
			} # resume warnings on uninitialized values

			# verify legal insertion
			my $level; # determine where we are from row ID
			if (defined $row->{'Term'}) {$level = 'Term'}
			elsif (defined $row->{'LangSet'}) {
				if (defined $term) { 
					error "LangSet-level row out of order in line $., skipped.";
					next;
				}
				$level = 'LangSet';
			}
			elsif (defined $row->{'Concept'}) {
				if (defined $langSet) {
					error "Concept-level row out of order in line $., skipped.";
					next;
				}
				$level = 'Concept';
			}
			else {die "Can't find level in row $., stopped"}
				# (can't happen)

			# is the datcat allowed at the level of the ID?
			unless ($legalIn{$level}{$row->{'DatCat'}}) {
				error "Data category '$row->{'DatCat'}' not allowed at the $level level in line $., skipped.";
				next;
			}

			# set $def if definition (legal only at langSet level)
			$def = 1 if ($row->{'DatCat'} eq 'definition');

			# bookkeeping: record links made
			push @linksMade, $row->{'Link'}->{'Value'} if (defined $row->{'Link'});

			# print item, or push into pre-tig list, depending
			if ($level eq 'Term') {
				push @unsortedTerm, $row;
			} else {
				printRow($row);
			}

		} # end if (in body, reading C-row)

		# not C-row: close any structures, segment = back
		if ($segment eq 'body' && !exists $row->{'Concept'}) {
			closeTerm();
			closeLangSet();
			closeConcept();
			print "</body>\n";
			$segment = 'back';
			print "<back>\n";
		}

		# if in back, R row?
		# R-row: separate parties, verify legality, stack it up
		if ($segment eq 'back' && exists $row->{'Party'}) {
			# have we changed parties?
			if (defined $party && $row->{'Party'} ne $party) {
				# change parties
				my $type;
				# what kind of party is the old one?
				my $topRow = shift @party;
				if ($topRow->{'DatCat'} eq 'type') {
					$type = $topRow->{'Value'};
				} else {
					unshift @party, $topRow;
					$type = 'unknown';
				}
				# file its info under its type and clean it out
				push @{$responsible{$type}}, [@party];
				undef @party;
			}
			# no? OK, add it to the current party.
			$party = $row->{'Party'}; # the party don't stop!
			# article says the first row must be type, but we can sort:
			if ($row->{'DatCat'} eq 'type') {
				unshift @party, $row;
			} else {
				push @party, $row;
			}
		} # end if (in back and reading R-row)

		# not R-row: warn file is misordered, last line
		# this code only runs if the A C R order is broken
		if ($segment eq 'back' && !exists $row->{'Party'}) {
			error "Don't know what to do with line $., processing stopped. Your file may be misordered. The line reads:\n$_";
			$aborted = 1;
			last;
		}

	} # end while (<$self->input_fh>)

	# finish up

	# if in body, close structures, body
	if ($segment eq 'body') {
		closeTerm();
		closeLangSet();
		closeConcept();
		print "</body>\n";
	}

	# if in back, sort and print parties, close back
	if ($segment eq 'back') {
		# file the last party under its type
		my $type;
		my $topRow = shift @party;
		if ($topRow->{'DatCat'} eq 'type') {
			$type = $topRow->{'Value'};
		} else {
			unshift @party, $topRow;
			$type = 'unknown';
		}
		push @{$responsible{$type}}, [@party];
		# print a refObjectList for each type of party,
		# within which each arrayref gets noted and printRow()ed.
		if (exists $responsible{'person'}) {
			print "<refObjectList type=\"respPerson\">\n";
			push @idsUsed, $_->[0]->{'ID'} foreach @{$responsible{'person'}};
			printRow($_) foreach @{$responsible{'person'}};
			print "</refObjectList>\n";
		}
		if (exists $responsible{'organization'}) {
			print "<refObjectList type=\"respOrg\">\n";
			push @idsUsed, $_->[0]->{'ID'} foreach @{$responsible{'organization'}};
			printRow($_) foreach @{$responsible{'organization'}};
			print "</refObjectList>\n";
		}
		if (exists $responsible{'unknown'}) {
			error "At least one of your responsible parties has no type (person, organization, etc.) and has been provisionally printed as a respParty. To conform to TBX-Basic, you must list each party as either a person or an organization.";
			print "<refObjectList type=\"respParty\">\n";
			push @idsUsed, $_->[0]->{'ID'} foreach @{$responsible{'unknown'}};
			printRow($_) foreach @{$responsible{'unknown'}};
			print "</refObjectList>\n";
		}
		print "</back>\n";
	}

	# closing formalities

	if (not $started) {
		my $err = "$ARGV does not contain a line beginning with =MRCTermTable. You must include such a line to switch on the TBX converter -- all preceding material is ignored.";
		
		print STDERR $err;
		error $err;
		
		close $self->tbx_fh;
		close $self->input_fh;
		return;
	}

	if ($aborted) {
		print STDERR "See $ARGV.warnings -- processing could not be completed.\n";
		close $self->tbx_fh;
		close $self->input_fh;
		return;
	}

	print "</text>\n</martif>\n";
	msg ("File includes links to: \n\t" . (join "\n\t", @linksMade))
		if @linksMade;

	msg "File includes IDs: \n\t" . (join "\n\t", @idsUsed)
		if @idsUsed;

	#print all messages to the object's log
	$self->log( Log::Message::Simple->stack_as_string() );
	Log::Message::Simple->flush();
	
	# TODO: is this necessary? also look for tbx_fh and input_fh
	# next open would close implicitly but not reset $.
	# close $self->log_fh;
	
	select $select;
}

# return a file suffix to ensure nothing is overwritten
sub _get_suffix {
	my ($file_name) = @_;
	my $suffix = "";
	$suffix-- while (-e "$file_name$suffix.tbx" or -e "$file_name$suffix.warnings"); 
	return $suffix;
}

sub closeTerm {
	if (defined $term) {
		my $id = $unsortedTerm[0]->{'ID'};
		my $tig = sortRefs(@unsortedTerm);
		my $posContext = pop @$tig;
		unless ($posContext || $def) {
			warn "Term $id is lacking an element necessary for TBX-Basic.\n\tTo make it valid for human use only, add one of:\n\t\ta definition (at the language level)\n\t\tan example of use in context (at the term level).\n\tTo make it valid for human or machine processing, add its part of speech (at the term level).\nSee line @{[$. - 1]}.\n\n";
		}
		printRow($tig);
		undef $term;
		undef @unsortedTerm;
	}
}

sub closeLangSet {
	if (defined $langSet) {
		print "</langSet>\n";
		undef $langSet;
		undef $def;
	}
}

sub closeConcept {
	if (defined $concept) {
		print "</termEntry>\n";
		undef $concept;
	}
}

sub parseRow {
	s/\s*$//; # super-chomp: cut off any trailing whitespace at all
	# later, split will eliminate between-field whitespace
	# and the keyword and langtag parsers will eliminate other space
	# outside of values
	return if /^$/; # skip lines that are all whitespace

	# fields are delimited by at least one tab and possibly spaces
	my @field = split / *(?:\t *)+/;

	# grab the three mandatory fields
	my %row;
	$row{'ID'} = shift @field;
	$row{'DatCat'} = shift @field;
	$row{'Value'} = shift @field;

	# verify essential completeness
	unless ($row{'ID'} && $row{'DatCat'} && $row{'Value'}) {
		warn "Incomplete row in line $., skipped.\n\n";
		return;
	}

	# verify well-formed ID and extract its semantics
	if ($row{'ID'} =~ /^[Cc] *(\d{3}) *($langCode)? *(\d*)$/) {
		if ($3 && !$2) {
			warn "Bad ID '$row{'ID'}' (no language section) in line $., skipped.\n\n";
			return;
		}
		$row{'Concept'} = $1;
		$row{'LangSet'} = "\L$2" if ($2); # smash to lowercase
		$row{'Term'} = 0 + $3 if ($2 && $3 ne ''); # cast to int
		# clean up the ID itself
		$row{'ID'} = "C$row{'Concept'}"; 
		$row{'ID'} .= $row{'LangSet'} if $row{'LangSet'};
		$row{'ID'} .= $row{'Term'} if defined $row{'Term'};
	} elsif ($row{'ID'} =~ /^[Rr] *(\d{3})$/) {
		$row{'Party'} = $1;
		$row{'ID'} = "R$1";
	} elsif ($row{'ID'} =~ /^[Aa]$/) {
		# this is a header line and okey-dokey
		$row{'ID'} = 'A';
	} else {
		warn "Bad ID '$row{'ID'}' (format not recognized) in line $., skipped.\n\n";
		return;
	}

	# correct case of the datcat, or warn and skip if can't match
	if (my $correct = $correctCaps{'DatCat'}{lc($row{'DatCat'})}) {
		# the datcat is recognized
		unless ($row{'DatCat'} eq $correct) {
			warn "Correcting '$row{'DatCat'}' to '$correct' in line $..\n\n";
			$row{'DatCat'} = $correct;
		}
	} else {
		warn "Unknown data category '$row{'DatCat'}' in line $., skipped.\n\n";
		return;
	}

	# parse off any local language override in Value
	if ($row{'Value'} =~ /^\[($langCode)] *(.*)$/) {
		$row{'RowLang'} = " xml:lang=\"\L$1\""; # lower case
		$row{'Value'} = $2;
	} # otherwise RowLang will (warn and) print nothing when asked

	# check certain Values against picklists and case-correct
	if ($row{'DatCat'} eq 'termLocation') {
		if (my $correct = $correctCaps{'termLocation'}{ lc($row{'Value'}) }) {
			# the value is a recognized termLocation
			unless ($row{'Value'} eq $correct) {
				warn "Correcting '$row{'Value'}' to '$correct' in line $..\n\n";
				$row{'Value'} = $correct;
			}
		} else {
			warn "Unfamiliar termLocation '$row{'Value'}' in line $.. If this is a location in a user interface, consult the suggested values in the TBX spec.\n\n";
			# but DON'T return undef, because this should not
			# lead to skipping the row, unlike other picklists
		}
	} elsif ($correctCaps{$row{'DatCat'}}) {
		my %caps = %{$correctCaps{$row{'DatCat'}}};
		# grab a correction hash appropriate to DatCat, 
		# if one exists
		if (my $correct = $caps{lc($row{'Value'})}) {
			unless ($row{'Value'} eq $correct) {
				warn "Correcting '$row{'Value'}' to '$correct' in line $..\n\n";
				$row{'Value'} = $correct;
			}
		} else {
			warn "'$row{'Value'}' not a valid $row{'DatCat'} in line $., skipped. See picklist for valid values.\n\n";
			return;
		}
	} # else it's not a correctible datcat, so let it be

	# get additional fields and language tags off of the row
	# forcing the keyword to one initial cap and prewriting the XMLattr
	foreach (@field) {
		my $keyword;
		if (/^([^:]+): *(?:\[($langCode)])? *(.+)$/) {
			$keyword = "\u\L$1";
			$row{$keyword}{'Value'} = $3;
			$row{$keyword}{'FieldLang'} = " xml:lang=\"\L$2\"" if $2;
		} else {
			warn "Can't parse additional field '$_' in line $., ignored.\n\n";
		}
		# check if a FieldLang makes sense
		if ($row{$keyword}{'FieldLang'} && !$allowed{$keyword}{'FieldLang'}) {
			warn "Language tag makes no sense with keyword '$keyword' in line $., ignored.\n\n";
			delete $row{$keyword}{'FieldLang'};
		}
		# check if this datcat can have this keyword
		# this bit might be better done in the controller?
		# heh. Too late now.
		unless ($allowed{$row{'DatCat'}}{$keyword}) {
			warn "Data category $row{'DatCat'} does not allow keyword '$keyword', ignored in line $..\n\n";
			if ($keyword eq 'Source' or $keyword eq 'Note') {
				warn "You may attach a source or note to an entire term entry (or a language section or concept entry) by placing it on its own line with the appropriate ID, like this: \n\t$row{ 'ID' }\t\l$keyword\t$row{ $keyword }{ 'Value' }\n\n";
			}
			delete $row{$keyword};
		}
	}

	# check for malformed Date
	if ($row{'Date'}) {
		if ($row{'Date'}{'Value'} =~ /^(\d{4})-(\d{2})-(\d{2})$/) {
			if ($1 eq '0000' || $2 eq '00' || $3 eq '00') {
				warn "Consider correcting: Zeroes in date '$row{'Date'}{'Value'}', line $..\n\n";
			} elsif ($2 < 13 && $3 < 13) {
				warn "Consider double-checking: Month and day are ambiguous in '$row{'Date'}{'Value'}', line $..\n\n";
			} elsif ($2 > 12) {
				warn "Consider correcting: Month $2 is nonsense in line $..\n\n";
			}
		} else {
			warn "Date '$row{'Date'}{'Value'}' not in ISO format (yyyy-mm-dd) in line $., ignored.\n\n";
			delete $row{'Date'};
		}
	}

	# check for Link where it's needed 
	if ($row{'DatCat'} eq 'transactionType') {
		warn "Consider adding information: No responsible party linked in line $..\n\n" unless $row{'Link'};
	} elsif ($row{'DatCat'} =~ /^(?:crossReference|externalCrossReference|xGraphic)$/ && !$row{'Link'}) {
		warn "$row{'DatCat'} without Link in line $., skipped.\n\n";
		return;
	}

	return \%row;
}

sub buildHeader {
	my ($srcRef, $destRef) = @_;
	my $destKey;
	return unless ($destKey = $corresp{ $srcRef->{ 'DatCat' }});
		# a validity check, not just a pointless translation
	if ($destKey eq 'Language' and defined $destRef->{'Language'}) {
		warn "Duplicate workingLanguage ignored in line $..\n\n"; 
		return;
	}
	push @{$destRef->{$destKey}}, $srcRef->{'Value'};
	return 1;
}

sub printHeader {
	my %info = %{shift;}; # that's a copy, but the hash is small
	return unless (defined $info{'Language'} && defined $info{'Source'});
	print <<REQUIRED1;
<?xml version='1.0' encoding="UTF-8"?>
<!DOCTYPE martif SYSTEM "TBXBasiccoreStructV02.dtd">
<martif type="TBX-Basic-V1" xml:lang="$info{'Language'}[0]">
<martifHeader>
<fileDesc>
<titleStmt>
<title>termbase from MRC file</title>
REQUIRED1

# print termbase-wide subjects, if such there be
	warn "Termbase-wide subject fields are recorded in the <titleStmt> element of the TBX header.\n\n"
		if (exists $info{'Subject'} and scalar @{ $info{'Subject'} } );
	my $sbj;
	print <<SUBJECT while $sbj = shift @{$info{'Subject'}};
<note>entire termbase concerns subject: $sbj</note>
SUBJECT

	print <<REQUIRED2;
</titleStmt>
<sourceDesc>
<p>generated by TBX::convert::MRC version $VERSION</p>
</sourceDesc>
REQUIRED2
	while (my $src = shift @{$info{'Source'}}) {
		print <<SOURCE;
<sourceDesc>
<p>$src</p>
</sourceDesc>
SOURCE
	}

	print <<REQUIRED3;
</fileDesc>
<encodingDesc>
<p type="DCSName">TBXBasicXCSV02.xcs</p>
REQUIRED3

#	my $sbj;
#	print <<SUBJECT while $sbj = shift @{$info{'Subject'}};
#<p type="subjectField">$sbj</p>
#SUBJECT

	print <<REQUIRED3;
</encodingDesc>
</martifHeader>
<text>
<body>
REQUIRED3
}

sub sortRefs {
	my (@termGrp, @auxInfo, $term, $pos, $context, $ID);
	$ID = $_[0]->{'ID'};
	while (my $row = shift) {
		my $datCat = $row->{'DatCat'};
		if ($datCat eq 'term') {
			unshift @termGrp, $row; # stick it on the front
			$term = 1;
		} elsif (my $position = $position{$datCat}) {
			if ('termGrp' eq $position) {
				push @termGrp, $row; # stick it on the back
				$pos = 1 if $datCat eq 'partOfSpeech';
			} elsif ('auxInfo' eq $position) {
				push @auxInfo, $row;
				$context = 1 if $datCat eq 'context';
			}
		} else {
			warn "Data category '$datCat' is not allowed at the term level.\n\n"; # other code should prevent this anyway
		}
	}
	warn "There is no term row for '$ID', although other data categories describe such a term. See line @{[$. - 1]}.\n\n"
		unless $term;
	warn "Term $ID lacks a partOfSpeech row. This TBX file may not be machine processed. See line @{[$. - 1]}.\n\n"
		unless $pos;
	unshift @auxInfo, \@termGrp;
	push @auxInfo, ($pos || $context); # 1 or undef
	return \@auxInfo;
}

sub printRow {
	no warnings 'uninitialized'; # for undefined language attributes
	my $item = shift;
	if (ref $item eq 'HASH') { # printing a single item
		# print as appropriate
		my $datCat;
		$datCat = $item->{'DatCat'};
		# sort by datcat
		if ($datCat eq 'term') {
			print "<term>$item->{'Value'}</term>\n";
			# we deliberately ignore RowLang, because LangSet
			# should give the language of a term entry
		}

		# note and source as standalones, not keyword-fields
		elsif ($datCat eq 'note') {
			print "<note$item->{'RowLang'}>$item->{'Value'}</note>\n";
		}

		elsif ($datCat =~ /^(?:source|customerSubset|projectSubset)$/) {
			print "<admin type=\"$datCat\"$item->{'RowLang'}>$item->{'Value'}</admin>\n";
		}

		# sorry this one's so gross, but it is
		elsif ($datCat eq 'transactionType') {
			print "<transacGrp>\n";
			print "\t<transac type=\"transactionType\">$item->{'Value'}</transac>\n";
			print "\t<date>$item->{'Date'}->{'Value'}</date>\n" if $item->{'Date'};
			print "\t<note$item->{'Note'}->{'FieldLang'}>$item->{'Note'}->{'Value'}</note>\n" if $item->{'Note'};
			if ($item->{'Responsibility'} || $item->{'Link'}) {
				print "\t<transacNote type=\"responsibility\"";
				print " target=\"$item->{'Link'}->{'Value'}\"" if $item->{'Link'};
				print "$item->{'Responsibility'}->{'FieldLang'}>$item->{'Responsibility'}->{'Value'}";
				print "Responsible Party" unless $item->{'Responsibility'}->{'Value'};
				print "</transacNote>\n";
			}
			print "</transacGrp>\n";
		}

		elsif ($datCat eq 'crossReference') {
			print "<ref type=\"crossReference\" target=\"$item->{'Link'}->{'Value'}\"$item->{'RowLang'}>$item->{'Value'}</ref>\n";
		}

		elsif ($datCat eq 'externalCrossReference' ||
			$datCat eq 'xGraphic') {
			print "<xref type=\"$datCat\" target=\"$item->{'Link'}->{'Value'}\"$item->{'RowLang'}>$item->{'Value'}</xref>\n";
		}

		elsif ($datCat =~ /^(?:email|title|role|org|uid|tel|adr|fn)$/) {
			print "\t<item type=\"$datCat\">$item->{'Value'}</item>\n";
			# RowLang is ignored here too -- attr not allowed
		}

		elsif ($meta{$datCat} eq 'termNote') {
			print "<termNote type=\"$datCat\"$item->{'RowLang'}>$item->{'Value'}</termNote>\n"; # using tigs means no termNoteGrp
		}

		else { # everything else is easy
			my $meta;
			$meta = $meta{$datCat} or die "printRow() can't print a $datCat "; # shouldn't happen
			print "<${meta}Grp>\n";
			print "\t<$meta type=\"$datCat\"$item->{'RowLang'}>$item->{'Value'}</$meta>\n";
			print "\t<note$item->{'Note'}->{'FieldLang'}>$item->{'Note'}->{'Value'}</note>\n" if $item->{'Note'};
			print "\t<admin type=\"source\"$item->{'Source'}->{'FieldLang'}>$item->{'Source'}->{'Value'}</admin>\n" if $item->{'Source'};
			print "</${meta}Grp>\n";
		}

	} elsif (ref $item eq 'ARRAY') {
		# if first item isn't arrayref, it's a resp-party
		if (ref $item->[0] ne 'ARRAY') { 
			print "<refObject id=\"$item->[0]->{'ID'}\">\n";
			printRow($_) foreach @$item;
			print "</refObject>\n";
		} else {
		# then it's a tig
			my $termGrp = shift @$item;
			my $id;
			if (exists $termGrp->[0]) {
			# if there's a term or any termNote
				$id = $termGrp->[0]->{'ID'}; 
			} else {
			# if must, get the ID from an auxInfo
			# (implies the input is defective)
				$id = $item->[0]->{'ID'};
			}
			print "<tig id=\"$id\">\n";
			# <termGrp> if this were an ntig
			printRow($_) foreach @$termGrp;
			# </termGrp>
			printRow($_) foreach @$item;
			print "</tig>\n";
		}
	} else {
		die "printRow() called incorrectly, stopped";
	}
}

