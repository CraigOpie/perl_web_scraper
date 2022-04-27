#!/usr/bin/perl
# Usage: perl opieca6.pl [dir]

use strict;
use warnings;

# Clear the STDOUT buffer
STDOUT->autoflush;

# Gather a list of html files in the current directory
my $dir = $ARGV[0];
opendir(DIR, $dir) or die "Can't open input directory: $!";
my @files = grep { /\.html/ } readdir DIR;
my @temp = grep { /\.html/ } readdir DIR;  # This is only here to get rid of a warning of possible issue due to only using DIR once
closedir DIR;

# Open output file
open my $out_data, '>', 'opieca6out.pl' or die "Can't open output file: $!";

# Prepare prolog file
my $prolog_header = '#!/usr/bin/env swipl
/*  Author:     Craig Opie
    Email:      opieca@hawaii.edu
    Assignment: No. 6
    References: https://www.swi-prolog.org/pldoc/doc_for?object=dif/2

    Test Cases: top([is, it, true, that, mark, hamill, acts, in, star, wars, iv]).
                top([who, acts, in, star, wars, iv]).
                top([did, mark, hamill, play, luke, skywalker]).
                top([mark, hamill, plays, luke, skywalker, right]).
                top([did, mark, hamill, play, han, solo, in, star, wars, iii]).
                top([did, mark, hamill, direct, star, wars, i]).
                top([is, it, true, that, mark, hamill, is, an, actor]).
                top([is, it, true, that, han, solo, is, a, character, in, star, wars, iii]).
                top([is, it, true, that, princess, leia, is, an, actor, and, mark, hamill, is, a, director]).
                top([george, lucas, is, a, director, and, mark, hamill, is, an, actor, and, han, solo, is, a, character, right]).
                top([who, is, the, actor, for, han, solo]).
                top([what, is, the, title, of, star, wars, ii]).
                top([who, is, the, director, of, star, wars, ii]).
                top([who, is, the, character, of, mark, hamill]).
*/
:- discontiguous proper_noun/3.
:- discontiguous is_a/2.
:- discontiguous char_in/2.
:- discontiguous character/1.
:- discontiguous actor/1.
:- discontiguous acts_in/2.
:- discontiguous plays/2.
:- discontiguous played_by/2.
:- discontiguous directed/2.

/*
    should write out a sensible answer regardless if it passes or fails
    it really should write out the text of the answer, not just the symbol.
*/
%% Overall sentence structures
top(Sentence) :- yesno(Query, Sentence, []), showresults(Query).
top(Sentence) :- who(Who, Sentence, []), write("The person youre looking for is "), write(Who). 
top(Sentence) :- what(What, Sentence, []), write("Youre looking for "), write(What).


/*
    writes out positive text if ARG1 is a list of true predicates, negative otherwise.
*/
%% Test if the value is true and write a statement as such, else write a statement that its not
showresults(Query) :- test(Query), write("Yes, thats true."), !.
showresults(_) :- write("Sorry, thats false."), !, fail.


/*
    takes a list of predicates and succeeds if all the predicates are true, otherwise fails.
*/
%% Perform logical tests on list
test([Query]) :- Query.
test([Query|Rest]) :- Query, test(Rest).


/*
    grammer section
*/
%% Who question breakdown and returns a list
who(X) --> [who, is, the], verb_phrase(X^_^[Query]), {Query}.
who(X) --> [who], verb_phrase(X^_^[Query]), {Query}.

%% What question breakdown and returns a list
what(X) --> [what, is, the], verb_phrase(X^_^[Query]), {Query}.
what(X) --> [what], verb_phrase(X^_^[Query]), {Query}.

%% True false statements that return true or false
yesno(Sem) --> [is, it, true, that], statement(_^_^Sem).
yesno(Sem) --> [is, the], statement(_^_^Sem).
yesno(Sem) --> [did], statement(_^_^Sem).
yesno(Sem) --> statement(_^_^Sem), [right].
yesno(Sem) --> statement(_^_^Sem).

%% Evaluates the statement based on structure
statement(S) --> singlestatement(S).
statement(_^_^Sem) --> singlestatement(_^_^S1), [and], statement(_^_^S2), {append(S1,S2,Sem)}.
statement(Subj^_^Sem) --> singlestatement(Subj^_^S1), prep_phrase(Subj^_^S2), {append(S1,S2,Sem)}.

%% Defines a single statement as consisting of a noun phrase followed by a verb phrase
singlestatement(Subj^Obj^Sem) --> noun_phrase(Subj), verb_phrase(Subj^Obj^Sem).

%% Defines a noun phrase consisting of a noun or proper noun
noun_phrase(Sem) --> noun(Sem).
noun_phrase(Sem) --> proper_noun(Sem).

%% Defines a verb phrase consisting of a verb followed by a noun phrase
verb_phrase(Subj^Obj^Sem) --> verb(Subj^Obj^Sem), noun_phrase(Obj).

%% Defines a prep phrase consisting of a verb followed by a noun phrase
prep_phrase(Subj^Obj^Sem) --> verb(Subj^Obj^Sem), noun_phrase(Obj).

%%  This section is defining director names
proper_noun(george_lucas) --> [george, lucas].
proper_noun(j_j_abrams) --> [j, j, abrams].
proper_noun(rian_johnson) --> [rian, johnson].

%%  This section is defining movie title names
proper_noun(star_wars1) --> [star, wars, i].
proper_noun(star_wars2) --> [star, wars, ii].
proper_noun(star_wars3) --> [star, wars, iii].
proper_noun(star_wars4) --> [star, wars, iv].
proper_noun(star_wars5) --> [star, wars, v].
proper_noun(star_wars6) --> [star, wars, vi].
proper_noun(star_wars7) --> [star, wars, vii].
proper_noun(star_wars8) --> [star, wars, viii].
proper_noun(star_wars9) --> [star, wars, ix].

%%  This section is defining the movie title names
proper_noun(the_phantom_menace) --> [the, phantom, menace].
proper_noun(attack_of_the_clones) --> [attack, of, the, clones].
proper_noun(revenge_of_the_sith) --> [revenge, of, the, sith].
proper_noun(a_new_hope) --> [a, new, hope].
proper_noun(the_empire_strikes_back) --> [the, empire, strikes, back].
proper_noun(return_of_the_jedi) --> [return, of, the, jedi].
proper_noun(the_force_awakens) --> [the, force, awakens].
proper_noun(the_last_jedi) --> [the, last, jedi].
proper_noun(the_rise_of_skywalker) --> [the, rise, of, skywalker].

%% Defines the nouns in the database
noun(actor) --> [actor].
noun(character) --> [character].
noun(director) --> [director].
noun(writer) --> [writer].

%% Defines the verbs in the database and assigns their function
verb(X^Y^[is_a(X,Y)]) --> [is, a].
verb(X^Y^[is_a(X,Y)]) --> [is, an].
verb(X^Y^[title(X,Y)]) --> [title, of].
verb(X^Y^[title(X,Y)]) --> [title].
verb(X^Y^[acts_in(X,Y)]) --> [acts, in].
verb(X^Y^[acts_in(X,Y)]) --> [act, in].
verb(X^Y^[acts_in(X,Y) ; char_in(X,Y)]) --> [in].
verb(X^Y^[plays(X,Y)]) --> [actor, for].
verb(X^Y^[played_by(X,Y)]) --> [character, of].
verb(X^Y^[plays(X,Y)]) --> [plays].
verb(X^Y^[plays(X,Y)]) --> [play].
verb(X^Y^[plays(X,Y)]) --> [played].
verb(X^Y^[wrote(X,Y)]) --> [wrote].
verb(X^Y^[wrote(X,Y)]) --> [writes].
verb(X^Y^[wrote(X,Y)]) --> [write].
verb(X^Y^[directed(X,Y)]) --> [director, of].
verb(X^Y^[directed(X,Y)]) --> [director].
verb(X^Y^[directed(X,Y)]) --> [directed].
verb(X^Y^[directed(X,Y)]) --> [directs].
verb(X^Y^[directed(X,Y)]) --> [direct].

%% Directors
director(george_lucas).
director(j_j_abrams).
director(rian_johnson).

%% Writers
writer(george_lucas).

%% Assings directors to films
directed(george_lucas, star_wars1).
directed(george_lucas, star_wars2).
directed(george_lucas, star_wars3).
directed(george_lucas, star_wars4).
directed(george_lucas, star_wars5).
directed(george_lucas, star_wars6).
directed(j_j_abrams, star_wars7).
directed(rian_johnson, star_wars8).
directed(j_j_abrams, star_wars9).

%% Assigns writers to films
wrote(george_lucas, star_wars1).
wrote(george_lucas, star_wars2).
wrote(george_lucas, star_wars3).
wrote(george_lucas, star_wars4).
wrote(george_lucas, star_wars5).
wrote(george_lucas, star_wars6).
wrote(j_j_abrams, star_wars7).
wrote(rian_johnson, star_wars8).
wrote(j_j_abrams, star_wars9).

%% Assigns titles to films
title(the_phantom_menace, star_wars1).
title(attack_of_the_clones, star_wars2).
title(revenge_of_the_sith, star_wars3).
title(a_new_hope, star_wars4).
title(the_empire_strikes_back, star_wars5).
title(return_of_the_jedi, star_wars6).
title(the_force_awakens, star_wars7).
title(the_last_jedi, star_wars8).
title(the_rise_of_skywalker, star_wars9).


/*
    database section
*/';
# print the pre-loaded info into the prolog file
print $out_data "$prolog_header\n";

# parse each file
foreach my $file(@files) {
    print "loading:   $file\n";
    open my $in_data, "<$file" or die "Can't open input file: $!";
    my @lines = <$in_data>;
    close $in_data;

    # Remove line breaks from lines
    chomp @lines;

    # Declare variables
    my @line = '';
    my $table_found = 0;
    my $is_director = 0;
    my $line_num = 0;
    my $name = '';
    my $name_ = '';
    my $namec = '';
    my $character = '';
    my $character_ = '';
    my $characterc = '';
    my $title = '';
    my $title_ = '';
    my $titlec = '';

    

    # Process Actor and Character Data
    for my $each_line (@lines) {
        # start of correct table
        if (index($each_line, '<table class="cast_list">') != -1) {
            $table_found = 1;
        }
        # process the actor's name
        if ((index($each_line, '<a href="https://www.imdb.com/name/') != -1) and (index($each_line, '<img height') == -1) and ($table_found == 1)) {
            @line = split(/>/, $each_line);
            $name = $line[1];
            $name =~ s/^\s+//;
            $name =~ s/\s+$//;
            $name =~ s/'//g;
            $name =~ s/\.//g;
            $name =~ s/\(//;
            $name =~ s/\)//;
            $name = lc $name;
            $name_ = $name;
            $namec = $name;
            $name_ =~ s/ /_/g;
            $namec =~ s/ /, /g;
            print $out_data "proper_noun($name_) --> [$namec].\n";
            print $out_data "actor($name_).\n";
            print $out_data "is_a($name_, actor).\n";
            print $out_data "acts_in($name_, star_wars4).\n";
        }
        # process the actor's character
        if ((index($each_line, '<td class="character">') != -1) and ($table_found == 1)) {
            @line = split(/>/, $lines[$line_num + 1]);
            @line = split(/</, $line[1]);
            $character = $line[0];
            $name =~ s/^\s+//;
            $name =~ s/\s+$//;
            $character =~ s/'//g;
            $character =~ s/\(//;
            $character =~ s/\)//;
            $character =~ s/#/no_/g;
            $character =~ s/\.//g;
            $character = lc $character;
            if (index($character, 'c-3po') != -1) {$character =~ s/-//;} 
            $character_ = $character;
            $characterc = $character;
            $character_ =~ s/ /_/g;
            $characterc =~ s/ /, /g;
            print $out_data "proper_noun($character_) --> [$characterc].\n";
            print $out_data "character($character_).\n";
            print $out_data "is_a($character_, character).\n";
            print $out_data "char_in($character_, star_wars4).\n";
            print $out_data "plays($name_, $character_).\n";
            print $out_data "played_by($character_, $name_).\n";

        }
        # end of table
        if ((index($each_line, '</table>') != -1) and ($table_found == 1)) {
            $table_found = 0;
            last;
        }
        $line_num += 1;
    }

    # reset variables between file types
    # Process individual information about actor
    $table_found = 0;
    $line_num = 0;  # probably a better way to do this, like with python but didn't put much effort into figuring it out
    for my $each_line (@lines) {
        # find the person's name
        if (index($each_line, '<h1 class="header"> <span class="itemprop">') != -1) {
            @line = split(/>/, $each_line);
            @line = split(/</, $line[2]);
            $name = $line[0];
            $name =~ s/^\s+//;
            $name =~ s/\s+$//;
            $name =~ s/'//g;
            $name =~ s/\.//g;
            $name =~ s/\,//g;
            $name =~ s/\(//g;
            $name =~ s/\)//g;
            $name =~ s/\- //g;
            $name =~ s/\!//g;
            $name =~ s/__/_/g;
            $name =~ s/_ / /g;
            $name =~ s/\&amp//g;
            $name = lc $name;
            $name_ = $name;
            $namec = $name;
            $name_ =~ s/ /_/g;
            $namec =~ s/ /, /g;
            print $out_data "proper_noun($name_) --> [$namec].\n";
            print $out_data "actor($name_).\n";
            print $out_data "is_a($name_, actor).\n";
        }
        # when in the correct table
        if (index($each_line, '<div id="filmography">') != -1) {
            $table_found = 1;
        }
        # process the movie or video titles
        if (((index($each_line, 'Movie)') != -1) or (index($each_line, 'Video)') != -1)) and ($table_found == 1)) {
            @line = split(/>/, $lines[$line_num-1]);
            @line = split(/</, $line[2]);
            $title = $line[0];
            $name =~ s/^\s+//;
            $name =~ s/\s+$//;
            $title =~ s/'//g;
            $title =~ s/\(//;
            $title =~ s/\)//;
            $title =~ s/#/no_/g;
            $title =~ s/\.//g;
            $title =~ s/\,//g;
            $title =~ s/\://g;
            $title =~ s/\;//g;
            $title =~ s/\!//g;
            $title =~ s/\- //;
            $title =~ s/\_\_/\_/g;
            $title =~ s/\_ / /g;
            $title =~ s/ \_/ /g;
            $title =~ s/\&amp//g;
            $title = lc $title;
            $title =~ tr/[⁰¹²³⁴⁵⁶⁷⁸⁹]/[0123456789]/;
            $title_ = $title;
            $titlec = $title;
            $title_ =~ s/ /_/g;
            $titlec =~ s/ /, /g;
            $title_ =~ s/\_\_/\_/g;
            $titlec =~ s/ \, / /g;
            print $out_data "acts_in($name_, $title_).\n";
            print $out_data "proper_noun($title_) --> [$titlec].\n";
        }
        # determine if the person is a director
        if (index($each_line, '<a name="director">Director</a>') != -1) {
            $is_director = 1;
            print $out_data "is_a($name_, director).\n";
        }
        # process works directed
        if ((index($each_line, 'id="director-') != -1) and ($is_director == 1)) {
            @line = split(/>/, $lines[$line_num+4]);
            @line = split(/</, $line[2]);
            $title = $line[0];
            $name =~ s/^\s+//;
            $name =~ s/\s+$//;
            $title =~ s/'//g;
            $title =~ s/\(//;
            $title =~ s/\)//;
            $title =~ s/#/no_/g;
            $title =~ s/\.//g;
            $title =~ s/\,//g;
            $title =~ s/\://g;
            $title =~ s/\;//g;
            $title =~ s/\!//g;
            $title =~ s/\- //;
            $title =~ s/\_\_/\_/g;
            $title =~ s/\_ / /g;
            $title =~ s/ \_/ /g;
            $title =~ s/\&amp//g;
            $title = lc $title;
            $title =~ tr/[⁰¹²³⁴⁵⁶⁷⁸⁹]/[0123456789]/;
            $title_ = $title;
            $titlec = $title;
            $title_ =~ s/ /_/g;
            $titlec =~ s/ /, /g;
            $title_ =~ s/\_\_/\_/g;
            $titlec =~ s/ \, / /g;
            print $out_data "directed($name_, $title_).\n";
        }
        # found the end of the table
        if ((index($each_line, '</table>') != -1) and ($table_found == 1)) {
            $table_found = 0;
            last;
        }
        $line_num += 1;
        
    }
    # notify the user that the information was successfully parsed
    print "completed: $file\n";
}

# close the output file
close $out_data;
