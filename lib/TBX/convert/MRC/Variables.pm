package TBX::convert::MRC::Variables;

# ABSTRACT: GLOBAL CONSTANTS USED BY TBX::Convert::MRC
# VERSION
use strict;
use warnings;
use base 'Exporter';
our @EXPORT = qw(
  %corresp
  $langCode
  %correctCaps
  %allowed
  %legalIn
  %position
  %meta
);

# reference variables
our %corresp     = _get_corresp();
our $langCode    = _get_lang_code();
our %correctCaps = _get_correct_caps();
our %allowed     = _get_allowed();
our %legalIn     = _get_legal_in();
our %position    = _get_position();
our %meta        = _get_meta();

# How does the data category from a header row relate to the header?
# (This is also a validity check.)
sub _get_corresp {
    my %corresp = (
        workingLanguage => 'Language',
        sourceDesc      => 'Source',
        subjectField    => 'Subject',
    );
    return %corresp;
}

# ISO 639 language code, and optionally region code: fr, eng-US
# case-insensitive; values are smashed to lowercase when parsed
sub _get_lang_code {
    return qr/[a-zA-Z]{2,3}(?:-[a-zA-Z]{2})?/;
}

# What is the proper capitalization for each data category/picklist item?
# A hash from a case-smashed version to the correct version, which will be
# used to recognize and fix user input.
sub _get_correct_caps {
    my %correct_caps;
    $correct_caps{'DatCat'}{ lc($_) } = $_ foreach qw (
      sourceDesc workingLanguage subjectField xGraphic definition
      term partOfSpeech administrativeStatus context geographicalUsage
      grammaticalGender termLocation termType note source
      crossReference externalCrossReference customerSubset projectSubset
      transactionType fn org title role email uid tel adr type
    );
    $correct_caps{'partOfSpeech'}{ lc($_) } = $_ foreach qw (
      noun verb adjective adverb properNoun other
    );
    $correct_caps{'administrativeStatus'}{ lc($_) } = $_ foreach qw (
      preferredTerm-admn-sts admittedTerm-admn-sts
      deprecatedTerm-admn-sts supersededTerm-admn-sts
    );
    $correct_caps{'grammaticalGender'}{ lc($_) } = $_ foreach qw (
      masculine feminine neuter other
    );
    $correct_caps{'termLocation'}{ lc($_) } = $_ foreach qw (
      menuItem dialogBox groupBox textBox comboBox comboBoxElement
      checkBox tab pushButton radioButton spinBox progressBar slider
      informativeMessage interactiveMessage toolTip tableText
      userDefinedType
    );
    $correct_caps{'termType'}{ lc($_) } = $_ foreach qw (
      fullForm acronym abbreviation shortForm variant phrase
    );
    $correct_caps{'transactionType'}{ lc($_) } = $_ foreach qw (
      origination modification
    );
    $correct_caps{'type'}{ lc($_) } = $_ foreach qw (
      person organization
    );
    return %correct_caps;
}

# Which additional fields are allowed on which data categories?
sub _get_allowed {
    my %allowed;
    $allowed{$_}{'Note'}   = 1 foreach qw ();
    $allowed{$_}{'Source'} = 1 foreach qw (
      definition subjectField context
    );
    $allowed{$_}{'Link'} = 1 foreach qw (
      transactionType crossReference externalCrossReference xGraphic
    );
    $allowed{'transactionType'}{'Date'} =
      $allowed{'transactionType'}{'Responsibility'} = 1;
    $allowed{$_}{'FieldLang'} = 1 foreach qw (Source Note Responsibility);
    return %allowed;
}

# which data categories are allowed at which level?
sub _get_legal_in {
    $legalIn{'Concept'}{$_} = 1 foreach qw (
      transactionType crossReference externalCrossReference
      customerSubset projectSubset xGraphic subjectField note
      source
    );
    $legalIn{'LangSet'}{$_} = 1 foreach qw (transactionType crossReference
      externalCrossReference customerSubset projectSubset
      definition note source
    );
    $legalIn{'Term'}{$_} = 1
      foreach qw (transactionType crossReference externalCrossReference
      customerSubset projectSubset context grammaticalGender
      geographicalUsage partOfSpeech termLocation termType
      administrativeStatus note source term
    );
    $legalIn{'Party'}{$_} = 1 foreach qw (email title role org uid tel adr fn);
    return %legalIn;
}

# what part of the term structure does each data category go in?
sub _get_position {
    my %position = map { $_ => 'termGrp' } qw (
      administrativeStatus geographicalUsage grammaticalGender
      partOfSpeech termLocation termType
    );
    %position = (
        %position,
        map { $_ => 'auxInfo' }
          qw (
          context customerSubset projectSubset crossReference note
          source transactionType externalCrossReference xGraphic
          )
    );
    return %position;
}

# which TBX meta data category does each data category print as?
sub _get_meta {
    my %meta;
    $meta{$_} = 'admin'   foreach qw (customerSubset projectSubset);
    $meta{$_} = 'descrip' foreach qw (definition subjectField context);
    $meta{$_} = 'termNote'
      foreach
      qw (grammaticalGender geographicalUsage partOfSpeech termLocation termType administrativeStatus);
    return %meta;
}

1;
