#!/usr/bin/env swipl
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
*/
proper_noun(mark_hamill) --> [mark, hamill].
actor(mark_hamill).
is_a(mark_hamill, actor).
acts_in(mark_hamill, star_wars4).
proper_noun(lukeskywalker) --> [lukeskywalker].
character(lukeskywalker).
is_a(lukeskywalker, character).
char_in(lukeskywalker, star_wars4).
plays(mark_hamill, lukeskywalker).
played_by(lukeskywalker, mark_hamill).
proper_noun(harrison_ford) --> [harrison, ford].
actor(harrison_ford).
is_a(harrison_ford, actor).
acts_in(harrison_ford, star_wars4).
proper_noun(hansolo) --> [hansolo].
character(hansolo).
is_a(hansolo, character).
char_in(hansolo, star_wars4).
plays(harrison_ford, hansolo).
played_by(hansolo, harrison_ford).
proper_noun(carrie_fisher) --> [carrie, fisher].
actor(carrie_fisher).
is_a(carrie_fisher, actor).
acts_in(carrie_fisher, star_wars4).
proper_noun(princessleia_organa) --> [princessleia, organa].
character(princessleia_organa).
is_a(princessleia_organa, character).
char_in(princessleia_organa, star_wars4).
plays(carrie_fisher, princessleia_organa).
played_by(princessleia_organa, carrie_fisher).
proper_noun(peter_cushing) --> [peter, cushing].
actor(peter_cushing).
is_a(peter_cushing, actor).
acts_in(peter_cushing, star_wars4).
proper_noun(grandmoff_tarkin) --> [grandmoff, tarkin].
character(grandmoff_tarkin).
is_a(grandmoff_tarkin, character).
char_in(grandmoff_tarkin, star_wars4).
plays(peter_cushing, grandmoff_tarkin).
played_by(grandmoff_tarkin, peter_cushing).
proper_noun(alec_guinness) --> [alec, guinness].
actor(alec_guinness).
is_a(alec_guinness, actor).
acts_in(alec_guinness, star_wars4).
proper_noun(benobi-wan_kenobi) --> [benobi-wan, kenobi].
character(benobi-wan_kenobi).
is_a(benobi-wan_kenobi, character).
char_in(benobi-wan_kenobi, star_wars4).
plays(alec_guinness, benobi-wan_kenobi).
played_by(benobi-wan_kenobi, alec_guinness).
proper_noun(anthony_daniels) --> [anthony, daniels].
actor(anthony_daniels).
is_a(anthony_daniels, actor).
acts_in(anthony_daniels, star_wars4).
proper_noun(c3po) --> [c3po].
character(c3po).
is_a(c3po, character).
char_in(c3po, star_wars4).
plays(anthony_daniels, c3po).
played_by(c3po, anthony_daniels).
proper_noun(kenny_baker) --> [kenny, baker].
actor(kenny_baker).
is_a(kenny_baker, actor).
acts_in(kenny_baker, star_wars4).
proper_noun(r2-d2) --> [r2-d2].
character(r2-d2).
is_a(r2-d2, character).
char_in(r2-d2, star_wars4).
plays(kenny_baker, r2-d2).
played_by(r2-d2, kenny_baker).
proper_noun(peter_mayhew) --> [peter, mayhew].
actor(peter_mayhew).
is_a(peter_mayhew, actor).
acts_in(peter_mayhew, star_wars4).
proper_noun(chewbacca) --> [chewbacca].
character(chewbacca).
is_a(chewbacca, character).
char_in(chewbacca, star_wars4).
plays(peter_mayhew, chewbacca).
played_by(chewbacca, peter_mayhew).
proper_noun(david_prowse) --> [david, prowse].
actor(david_prowse).
is_a(david_prowse, actor).
acts_in(david_prowse, star_wars4).
proper_noun(darthvader) --> [darthvader].
character(darthvader).
is_a(darthvader, character).
char_in(darthvader, star_wars4).
plays(david_prowse, darthvader).
played_by(darthvader, david_prowse).
proper_noun(phil_brown) --> [phil, brown].
actor(phil_brown).
is_a(phil_brown, actor).
acts_in(phil_brown, star_wars4).
proper_noun(uncleowen) --> [uncleowen].
character(uncleowen).
is_a(uncleowen, character).
char_in(uncleowen, star_wars4).
plays(phil_brown, uncleowen).
played_by(uncleowen, phil_brown).
proper_noun(shelagh_fraser) --> [shelagh, fraser].
actor(shelagh_fraser).
is_a(shelagh_fraser, actor).
acts_in(shelagh_fraser, star_wars4).
proper_noun(auntberu) --> [auntberu].
character(auntberu).
is_a(auntberu, character).
char_in(auntberu, star_wars4).
plays(shelagh_fraser, auntberu).
played_by(auntberu, shelagh_fraser).
proper_noun(jack_purvis) --> [jack, purvis].
actor(jack_purvis).
is_a(jack_purvis, actor).
acts_in(jack_purvis, star_wars4).
proper_noun(chiefjawa) --> [chiefjawa].
character(chiefjawa).
is_a(chiefjawa, character).
char_in(chiefjawa, star_wars4).
plays(jack_purvis, chiefjawa).
played_by(chiefjawa, jack_purvis).
proper_noun(alex_mccrindle) --> [alex, mccrindle].
actor(alex_mccrindle).
is_a(alex_mccrindle, actor).
acts_in(alex_mccrindle, star_wars4).
proper_noun(generaldodonna) --> [generaldodonna].
character(generaldodonna).
is_a(generaldodonna, character).
char_in(generaldodonna, star_wars4).
plays(alex_mccrindle, generaldodonna).
played_by(generaldodonna, alex_mccrindle).
proper_noun(eddie_byrne) --> [eddie, byrne].
actor(eddie_byrne).
is_a(eddie_byrne, actor).
acts_in(eddie_byrne, star_wars4).
proper_noun(generalwillard) --> [generalwillard].
character(generalwillard).
is_a(generalwillard, character).
char_in(generalwillard, star_wars4).
plays(eddie_byrne, generalwillard).
played_by(generalwillard, eddie_byrne).
proper_noun(drewe_henley) --> [drewe, henley].
actor(drewe_henley).
is_a(drewe_henley, actor).
acts_in(drewe_henley, star_wars4).
proper_noun(redleader) --> [redleader].
character(redleader).
is_a(redleader, character).
char_in(redleader, star_wars4).
plays(drewe_henley, redleader).
played_by(redleader, drewe_henley).
proper_noun(denis_lawson) --> [denis, lawson].
actor(denis_lawson).
is_a(denis_lawson, actor).
acts_in(denis_lawson, star_wars4).
proper_noun(redtwo_wedge) --> [redtwo, wedge].
character(redtwo_wedge).
is_a(redtwo_wedge, character).
char_in(redtwo_wedge, star_wars4).
plays(denis_lawson, redtwo_wedge).
played_by(redtwo_wedge, denis_lawson).
proper_noun(garrick_hagon) --> [garrick, hagon].
actor(garrick_hagon).
is_a(garrick_hagon, actor).
acts_in(garrick_hagon, star_wars4).
proper_noun(redthree_biggs) --> [redthree, biggs].
character(redthree_biggs).
is_a(redthree_biggs, character).
char_in(redthree_biggs, star_wars4).
plays(garrick_hagon, redthree_biggs).
played_by(redthree_biggs, garrick_hagon).
proper_noun(jack_klaff) --> [jack, klaff].
actor(jack_klaff).
is_a(jack_klaff, actor).
acts_in(jack_klaff, star_wars4).
proper_noun(redfour_john_d) --> [redfour, john, d].
character(redfour_john_d).
is_a(redfour_john_d, character).
char_in(redfour_john_d, star_wars4).
plays(jack_klaff, redfour_john_d).
played_by(redfour_john_d, jack_klaff).
proper_noun(william_hootkins) --> [william, hootkins].
actor(william_hootkins).
is_a(william_hootkins, actor).
acts_in(william_hootkins, star_wars4).
proper_noun(redsix_porkins) --> [redsix, porkins].
character(redsix_porkins).
is_a(redsix_porkins, character).
char_in(redsix_porkins, star_wars4).
plays(william_hootkins, redsix_porkins).
played_by(redsix_porkins, william_hootkins).
proper_noun(angus_macinnes) --> [angus, macinnes].
actor(angus_macinnes).
is_a(angus_macinnes, actor).
acts_in(angus_macinnes, star_wars4).
proper_noun(goldleader) --> [goldleader].
character(goldleader).
is_a(goldleader, character).
char_in(goldleader, star_wars4).
plays(angus_macinnes, goldleader).
played_by(goldleader, angus_macinnes).
proper_noun(jeremy_sinden) --> [jeremy, sinden].
actor(jeremy_sinden).
is_a(jeremy_sinden, actor).
acts_in(jeremy_sinden, star_wars4).
proper_noun(goldtwo) --> [goldtwo].
character(goldtwo).
is_a(goldtwo, character).
char_in(goldtwo, star_wars4).
plays(jeremy_sinden, goldtwo).
played_by(goldtwo, jeremy_sinden).
proper_noun(graham_ashley) --> [graham, ashley].
actor(graham_ashley).
is_a(graham_ashley, actor).
acts_in(graham_ashley, star_wars4).
proper_noun(goldfive) --> [goldfive].
character(goldfive).
is_a(goldfive, character).
char_in(goldfive, star_wars4).
plays(graham_ashley, goldfive).
played_by(goldfive, graham_ashley).
proper_noun(don_henderson) --> [don, henderson].
actor(don_henderson).
is_a(don_henderson, actor).
acts_in(don_henderson, star_wars4).
proper_noun(generaltaggi) --> [generaltaggi].
character(generaltaggi).
is_a(generaltaggi, character).
char_in(generaltaggi, star_wars4).
plays(don_henderson, generaltaggi).
played_by(generaltaggi, don_henderson).
proper_noun(richard_leparmentier) --> [richard, leparmentier].
actor(richard_leparmentier).
is_a(richard_leparmentier, actor).
acts_in(richard_leparmentier, star_wars4).
proper_noun(generalmotti) --> [generalmotti].
character(generalmotti).
is_a(generalmotti, character).
char_in(generalmotti, star_wars4).
plays(richard_leparmentier, generalmotti).
played_by(generalmotti, richard_leparmentier).
proper_noun(leslie_schofield) --> [leslie, schofield].
actor(leslie_schofield).
is_a(leslie_schofield, actor).
acts_in(leslie_schofield, star_wars4).
proper_noun(commanderno_1) --> [commanderno_1].
character(commanderno_1).
is_a(commanderno_1, character).
char_in(commanderno_1, star_wars4).
plays(leslie_schofield, commanderno_1).
played_by(commanderno_1, leslie_schofield).
proper_noun(david_ankrum) --> [david, ankrum].
actor(david_ankrum).
is_a(david_ankrum, actor).
acts_in(david_ankrum, star_wars4).
proper_noun(redtwo) --> [redtwo].
character(redtwo).
is_a(redtwo, character).
char_in(redtwo, star_wars4).
plays(david_ankrum, redtwo).
played_by(redtwo, david_ankrum).
proper_noun(mark_anthony_austin) --> [mark, anthony, austin].
actor(mark_anthony_austin).
is_a(mark_anthony_austin, actor).
acts_in(mark_anthony_austin, star_wars4).
proper_noun(bobafett_special_edition) --> [bobafett, special, edition].
character(bobafett_special_edition).
is_a(bobafett_special_edition, character).
char_in(bobafett_special_edition, star_wars4).
plays(mark_anthony_austin, bobafett_special_edition).
played_by(bobafett_special_edition, mark_anthony_austin).
proper_noun(scott_beach) --> [scott, beach].
actor(scott_beach).
is_a(scott_beach, actor).
acts_in(scott_beach, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(scott_beach, stormtrooper).
played_by(stormtrooper, scott_beach).
proper_noun(lightning_bear) --> [lightning, bear].
actor(lightning_bear).
is_a(lightning_bear, actor).
acts_in(lightning_bear, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(lightning_bear, stormtrooper).
played_by(stormtrooper, lightning_bear).
proper_noun(jon_berg) --> [jon, berg].
actor(jon_berg).
is_a(jon_berg, actor).
acts_in(jon_berg, star_wars4).
proper_noun(cantinaalien) --> [cantinaalien].
character(cantinaalien).
is_a(cantinaalien, character).
char_in(cantinaalien, star_wars4).
plays(jon_berg, cantinaalien).
played_by(cantinaalien, jon_berg).
proper_noun(doug_beswick) --> [doug, beswick].
actor(doug_beswick).
is_a(doug_beswick, actor).
acts_in(doug_beswick, star_wars4).
proper_noun(cantinaalien) --> [cantinaalien].
character(cantinaalien).
is_a(cantinaalien, character).
char_in(cantinaalien, star_wars4).
plays(doug_beswick, cantinaalien).
played_by(cantinaalien, doug_beswick).
proper_noun(paul_blake) --> [paul, blake].
actor(paul_blake).
is_a(paul_blake, actor).
acts_in(paul_blake, star_wars4).
proper_noun(greedo) --> [greedo].
character(greedo).
is_a(greedo, character).
char_in(greedo, star_wars4).
plays(paul_blake, greedo).
played_by(greedo, paul_blake).
proper_noun(janice_burchette) --> [janice, burchette].
actor(janice_burchette).
is_a(janice_burchette, actor).
acts_in(janice_burchette, star_wars4).
proper_noun(nabrunleids) --> [nabrunleids].
character(nabrunleids).
is_a(nabrunleids, character).
char_in(nabrunleids, star_wars4).
plays(janice_burchette, nabrunleids).
played_by(nabrunleids, janice_burchette).
proper_noun(ted_burnett) --> [ted, burnett].
actor(ted_burnett).
is_a(ted_burnett, actor).
acts_in(ted_burnett, star_wars4).
proper_noun(wuher) --> [wuher].
character(wuher).
is_a(wuher, character).
char_in(wuher, star_wars4).
plays(ted_burnett, wuher).
played_by(wuher, ted_burnett).
proper_noun(john_chapman) --> [john, chapman].
actor(john_chapman).
is_a(john_chapman, actor).
acts_in(john_chapman, star_wars4).
proper_noun(drifterred_12) --> [drifterred, 12].
character(drifterred_12).
is_a(drifterred_12, character).
char_in(drifterred_12, star_wars4).
plays(john_chapman, drifterred_12).
played_by(drifterred_12, john_chapman).
proper_noun(gilda_cohen) --> [gilda, cohen].
actor(gilda_cohen).
is_a(gilda_cohen, actor).
acts_in(gilda_cohen, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(gilda_cohen, cantinapatron).
played_by(cantinapatron, gilda_cohen).
proper_noun(tim_condren) --> [tim, condren].
actor(tim_condren).
is_a(tim_condren, actor).
acts_in(tim_condren, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(tim_condren, stormtrooper).
played_by(stormtrooper, tim_condren).
proper_noun(barry_copping) --> [barry, copping].
actor(barry_copping).
is_a(barry_copping, actor).
acts_in(barry_copping, star_wars4).
proper_noun(wioslea) --> [wioslea].
character(wioslea).
is_a(wioslea, character).
char_in(wioslea, star_wars4).
plays(barry_copping, wioslea).
played_by(wioslea, barry_copping).
proper_noun(alfie_curtis) --> [alfie, curtis].
actor(alfie_curtis).
is_a(alfie_curtis, actor).
acts_in(alfie_curtis, star_wars4).
proper_noun(drevazan) --> [drevazan].
character(drevazan).
is_a(drevazan, character).
char_in(drevazan, star_wars4).
plays(alfie_curtis, drevazan).
played_by(drevazan, alfie_curtis).
proper_noun(robert_davies) --> [robert, davies].
actor(robert_davies).
is_a(robert_davies, actor).
acts_in(robert_davies, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(robert_davies, cantinapatron).
played_by(cantinapatron, robert_davies).
proper_noun(maria_de_aragon) --> [maria, de, aragon].
actor(maria_de_aragon).
is_a(maria_de_aragon, actor).
acts_in(maria_de_aragon, star_wars4).
proper_noun(greedo) --> [greedo].
character(greedo).
is_a(greedo, character).
char_in(greedo, star_wars4).
plays(maria_de_aragon, greedo).
played_by(greedo, maria_de_aragon).
proper_noun(barbie_denham) --> [barbie, denham].
actor(barbie_denham).
is_a(barbie_denham, actor).
acts_in(barbie_denham, star_wars4).
proper_noun(hrchekkal_fas) --> [hrchekkal, fas].
character(hrchekkal_fas).
is_a(hrchekkal_fas, character).
char_in(hrchekkal_fas, star_wars4).
plays(barbie_denham, hrchekkal_fas).
played_by(hrchekkal_fas, barbie_denham).
proper_noun(frazer_diamond) --> [frazer, diamond].
actor(frazer_diamond).
is_a(frazer_diamond, actor).
acts_in(frazer_diamond, star_wars4).
proper_noun(jawa) --> [jawa].
character(jawa).
is_a(jawa, character).
char_in(jawa, star_wars4).
plays(frazer_diamond, jawa).
played_by(jawa, frazer_diamond).
proper_noun(peter_diamond) --> [peter, diamond].
actor(peter_diamond).
is_a(peter_diamond, actor).
acts_in(peter_diamond, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(peter_diamond, stormtrooper).
played_by(stormtrooper, peter_diamond).
proper_noun(warwick_diamond) --> [warwick, diamond].
actor(warwick_diamond).
is_a(warwick_diamond, actor).
acts_in(warwick_diamond, star_wars4).
proper_noun(jawa) --> [jawa].
character(jawa).
is_a(jawa, character).
char_in(jawa, star_wars4).
plays(warwick_diamond, jawa).
played_by(jawa, warwick_diamond).
proper_noun(sadie_eden) --> [sadie, eden].
actor(sadie_eden).
is_a(sadie_eden, actor).
acts_in(sadie_eden, star_wars4).
proper_noun(garindan) --> [garindan].
character(garindan).
is_a(garindan, character).
char_in(garindan, star_wars4).
plays(sadie_eden, garindan).
played_by(garindan, sadie_eden).
proper_noun(kim_falkinburg) --> [kim, falkinburg].
actor(kim_falkinburg).
is_a(kim_falkinburg, actor).
acts_in(kim_falkinburg, star_wars4).
proper_noun(djaspuhr) --> [djaspuhr].
character(djaspuhr).
is_a(djaspuhr, character).
char_in(djaspuhr, star_wars4).
plays(kim_falkinburg, djaspuhr).
played_by(djaspuhr, kim_falkinburg).
proper_noun(harry_fielder) --> [harry, fielder].
actor(harry_fielder).
is_a(harry_fielder, actor).
acts_in(harry_fielder, star_wars4).
proper_noun(deathstar_trooper) --> [deathstar, trooper].
character(deathstar_trooper).
is_a(deathstar_trooper, character).
char_in(deathstar_trooper, star_wars4).
plays(harry_fielder, deathstar_trooper).
played_by(deathstar_trooper, harry_fielder).
proper_noun(anthony_forrest) --> [anthony, forrest].
actor(anthony_forrest).
is_a(anthony_forrest, actor).
acts_in(anthony_forrest, star_wars4).
proper_noun(sandtrooper) --> [sandtrooper].
character(sandtrooper).
is_a(sandtrooper, character).
char_in(sandtrooper, star_wars4).
plays(anthony_forrest, sandtrooper).
played_by(sandtrooper, anthony_forrest).
proper_noun(ted_gagliano) --> [ted, gagliano].
actor(ted_gagliano).
is_a(ted_gagliano, actor).
acts_in(ted_gagliano, star_wars4).
proper_noun(stormtrooperwith_binoculars) --> [stormtrooperwith, binoculars].
character(stormtrooperwith_binoculars).
is_a(stormtrooperwith_binoculars, character).
char_in(stormtrooperwith_binoculars, star_wars4).
plays(ted_gagliano, stormtrooperwith_binoculars).
played_by(stormtrooperwith_binoculars, ted_gagliano).
proper_noun(salo_gardner) --> [salo, gardner].
actor(salo_gardner).
is_a(salo_gardner, actor).
acts_in(salo_gardner, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(salo_gardner, cantinapatron).
played_by(cantinapatron, salo_gardner).
proper_noun(steve_gawley) --> [steve, gawley].
actor(steve_gawley).
is_a(steve_gawley, actor).
acts_in(steve_gawley, star_wars4).
proper_noun(deathstar_trooper) --> [deathstar, trooper].
character(deathstar_trooper).
is_a(deathstar_trooper, character).
char_in(deathstar_trooper, star_wars4).
plays(steve_gawley, deathstar_trooper).
played_by(deathstar_trooper, steve_gawley).
proper_noun(barry_gnome) --> [barry, gnome].
actor(barry_gnome).
is_a(barry_gnome, actor).
acts_in(barry_gnome, star_wars4).
proper_noun(kabe) --> [kabe].
character(kabe).
is_a(kabe, character).
char_in(kabe, star_wars4).
plays(barry_gnome, kabe).
played_by(kabe, barry_gnome).
proper_noun(rusty_goffe) --> [rusty, goffe].
actor(rusty_goffe).
is_a(rusty_goffe, actor).
acts_in(rusty_goffe, star_wars4).
proper_noun(kabe) --> [kabe].
character(kabe).
is_a(kabe, character).
char_in(kabe, star_wars4).
plays(rusty_goffe, kabe).
played_by(kabe, rusty_goffe).
proper_noun(isaac_grand) --> [isaac, grand].
actor(isaac_grand).
is_a(isaac_grand, actor).
acts_in(isaac_grand, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(isaac_grand, cantinapatron).
played_by(cantinapatron, isaac_grand).
proper_noun(nelson_hall) --> [nelson, hall].
actor(nelson_hall).
is_a(nelson_hall, actor).
acts_in(nelson_hall, star_wars4).
proper_noun(stormtrooperspecial_edition) --> [stormtrooperspecial, edition].
character(stormtrooperspecial_edition).
is_a(stormtrooperspecial_edition, character).
char_in(stormtrooperspecial_edition, star_wars4).
plays(nelson_hall, stormtrooperspecial_edition).
played_by(stormtrooperspecial_edition, nelson_hall).
proper_noun(reg_harding) --> [reg, harding].
actor(reg_harding).
is_a(reg_harding, actor).
acts_in(reg_harding, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(reg_harding, stormtrooper).
played_by(stormtrooper, reg_harding).
proper_noun(alan_harris) --> [alan, harris].
actor(alan_harris).
is_a(alan_harris, actor).
acts_in(alan_harris, star_wars4).
proper_noun(leiasrebel_escort) --> [leiasrebel, escort].
character(leiasrebel_escort).
is_a(leiasrebel_escort, character).
char_in(leiasrebel_escort, star_wars4).
plays(alan_harris, leiasrebel_escort).
played_by(leiasrebel_escort, alan_harris).
proper_noun(frank_henson) --> [frank, henson].
actor(frank_henson).
is_a(frank_henson, actor).
acts_in(frank_henson, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(frank_henson, stormtrooper).
played_by(stormtrooper, frank_henson).
proper_noun(christine_hewett) --> [christine, hewett].
actor(christine_hewett).
is_a(christine_hewett, actor).
acts_in(christine_hewett, star_wars4).
proper_noun(breatonnika) --> [breatonnika].
character(breatonnika).
is_a(breatonnika, character).
char_in(breatonnika, star_wars4).
plays(christine_hewett, breatonnika).
played_by(breatonnika, christine_hewett).
proper_noun(tiffany_hillkurtz) --> [tiffany, hillkurtz].
actor(tiffany_hillkurtz).
is_a(tiffany_hillkurtz, actor).
acts_in(tiffany_hillkurtz, star_wars4).
proper_noun(jawa) --> [jawa].
character(jawa).
is_a(jawa, character).
char_in(jawa, star_wars4).
plays(tiffany_hillkurtz, jawa).
played_by(jawa, tiffany_hillkurtz).
proper_noun(arthur_howell) --> [arthur, howell].
actor(arthur_howell).
is_a(arthur_howell, actor).
acts_in(arthur_howell, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(arthur_howell, stormtrooper).
played_by(stormtrooper, arthur_howell).
proper_noun(tommy_ilsley) --> [tommy, ilsley].
actor(tommy_ilsley).
is_a(tommy_ilsley, actor).
acts_in(tommy_ilsley, star_wars4).
proper_noun(pondababa) --> [pondababa].
character(pondababa).
is_a(pondababa, character).
char_in(pondababa, star_wars4).
plays(tommy_ilsley, pondababa).
played_by(pondababa, tommy_ilsley).
proper_noun(joe_johnston) --> [joe, johnston].
actor(joe_johnston).
is_a(joe_johnston, actor).
acts_in(joe_johnston, star_wars4).
proper_noun(deathstar_trooper) --> [deathstar, trooper].
character(deathstar_trooper).
is_a(deathstar_trooper, character).
char_in(deathstar_trooper, star_wars4).
plays(joe_johnston, deathstar_trooper).
played_by(deathstar_trooper, joe_johnston).
proper_noun(annette_jones) --> [annette, jones].
actor(annette_jones).
is_a(annette_jones, actor).
acts_in(annette_jones, star_wars4).
proper_noun(mosep) --> [mosep].
character(mosep).
is_a(mosep, character).
char_in(mosep, star_wars4).
plays(annette_jones, mosep).
played_by(mosep, annette_jones).
proper_noun(james_earl_jones) --> [james, earl, jones].
actor(james_earl_jones).
is_a(james_earl_jones, actor).
acts_in(james_earl_jones, star_wars4).
proper_noun(darthvader) --> [darthvader].
character(darthvader).
is_a(darthvader, character).
char_in(darthvader, star_wars4).
plays(james_earl_jones, darthvader).
played_by(darthvader, james_earl_jones).
proper_noun(linda_jones) --> [linda, jones].
actor(linda_jones).
is_a(linda_jones, actor).
acts_in(linda_jones, star_wars4).
proper_noun(challbekan) --> [challbekan].
character(challbekan).
is_a(challbekan, character).
char_in(challbekan, star_wars4).
plays(linda_jones, challbekan).
played_by(challbekan, linda_jones).
proper_noun(joe_kaye) --> [joe, kaye].
actor(joe_kaye).
is_a(joe_kaye, actor).
acts_in(joe_kaye, star_wars4).
proper_noun(solomohal) --> [solomohal].
character(solomohal).
is_a(solomohal, character).
char_in(solomohal, star_wars4).
plays(joe_kaye, solomohal).
played_by(solomohal, joe_kaye).
proper_noun(colin_michael_kitchens) --> [colin, michael, kitchens].
actor(colin_michael_kitchens).
is_a(colin_michael_kitchens, actor).
acts_in(colin_michael_kitchens, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(colin_michael_kitchens, stormtrooper).
played_by(stormtrooper, colin_michael_kitchens).
proper_noun(melissa_kurtz) --> [melissa, kurtz].
actor(melissa_kurtz).
is_a(melissa_kurtz, actor).
acts_in(melissa_kurtz, star_wars4).
proper_noun(jawa) --> [jawa].
character(jawa).
is_a(jawa, character).
char_in(jawa, star_wars4).
plays(melissa_kurtz, jawa).
played_by(jawa, melissa_kurtz).
proper_noun(al_lampert) --> [al, lampert].
actor(al_lampert).
is_a(al_lampert, actor).
acts_in(al_lampert, star_wars4).
proper_noun(dainejir) --> [dainejir].
character(dainejir).
is_a(dainejir, character).
char_in(dainejir, star_wars4).
plays(al_lampert, dainejir).
played_by(dainejir, al_lampert).
proper_noun(laine_liska) --> [laine, liska].
actor(laine_liska).
is_a(laine_liska, actor).
acts_in(laine_liska, star_wars4).
proper_noun(muftak) --> [muftak].
character(muftak).
is_a(muftak, character).
char_in(muftak, star_wars4).
plays(laine_liska, muftak).
played_by(muftak, laine_liska).
proper_noun(derek_lyons) --> [derek, lyons].
actor(derek_lyons).
is_a(derek_lyons, actor).
acts_in(derek_lyons, star_wars4).
proper_noun(templeguard) --> [templeguard].
character(templeguard).
is_a(templeguard, character).
char_in(templeguard, star_wars4).
plays(derek_lyons, templeguard).
played_by(templeguard, derek_lyons).
proper_noun(mahjoub) --> [mahjoub].
actor(mahjoub).
is_a(mahjoub, actor).
acts_in(mahjoub, star_wars4).
proper_noun(jawa) --> [jawa].
character(jawa).
is_a(jawa, character).
char_in(jawa, star_wars4).
plays(mahjoub, jawa).
played_by(jawa, mahjoub).
proper_noun(alf_mangan) --> [alf, mangan].
actor(alf_mangan).
is_a(alf_mangan, actor).
acts_in(alf_mangan, star_wars4).
proper_noun(takeel) --> [takeel].
character(takeel).
is_a(takeel, character).
char_in(takeel, star_wars4).
plays(alf_mangan, takeel).
played_by(takeel, alf_mangan).
proper_noun(rick_mccallum) --> [rick, mccallum].
actor(rick_mccallum).
is_a(rick_mccallum, actor).
acts_in(rick_mccallum, star_wars4).
proper_noun(stormtrooperspecial_edition) --> [stormtrooperspecial, edition].
character(stormtrooperspecial_edition).
is_a(stormtrooperspecial_edition, character).
char_in(stormtrooperspecial_edition, star_wars4).
plays(rick_mccallum, stormtrooperspecial_edition).
played_by(stormtrooperspecial_edition, rick_mccallum).
proper_noun(grant_mccune) --> [grant, mccune].
actor(grant_mccune).
is_a(grant_mccune, actor).
acts_in(grant_mccune, star_wars4).
proper_noun(deathstar_gunner) --> [deathstar, gunner].
character(deathstar_gunner).
is_a(deathstar_gunner, character).
char_in(deathstar_gunner, star_wars4).
plays(grant_mccune, deathstar_gunner).
played_by(deathstar_gunner, grant_mccune).
proper_noun(geoffrey_moon) --> [geoffrey, moon].
actor(geoffrey_moon).
is_a(geoffrey_moon, actor).
acts_in(geoffrey_moon, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(geoffrey_moon, cantinapatron).
played_by(cantinapatron, geoffrey_moon).
proper_noun(mandy_morton) --> [mandy, morton].
actor(mandy_morton).
is_a(mandy_morton, actor).
acts_in(mandy_morton, star_wars4).
proper_noun(swillacorey) --> [swillacorey].
character(swillacorey).
is_a(swillacorey, character).
char_in(swillacorey, star_wars4).
plays(mandy_morton, swillacorey).
played_by(swillacorey, mandy_morton).
proper_noun(lorne_peterson) --> [lorne, peterson].
actor(lorne_peterson).
is_a(lorne_peterson, actor).
acts_in(lorne_peterson, star_wars4).
proper_noun(massassibase_rebel_scout) --> [massassibase, rebel, scout].
character(massassibase_rebel_scout).
is_a(massassibase_rebel_scout, character).
char_in(massassibase_rebel_scout, star_wars4).
plays(lorne_peterson, massassibase_rebel_scout).
played_by(massassibase_rebel_scout, lorne_peterson).
proper_noun(marcus_powell) --> [marcus, powell].
actor(marcus_powell).
is_a(marcus_powell, actor).
acts_in(marcus_powell, star_wars4).
proper_noun(rycarryjerd) --> [rycarryjerd].
character(rycarryjerd).
is_a(rycarryjerd, character).
char_in(rycarryjerd, star_wars4).
plays(marcus_powell, rycarryjerd).
played_by(rycarryjerd, marcus_powell).
proper_noun(shane_rimmer) --> [shane, rimmer].
actor(shane_rimmer).
is_a(shane_rimmer, actor).
acts_in(shane_rimmer, star_wars4).
proper_noun(incomengineer) --> [incomengineer].
character(incomengineer).
is_a(incomengineer, character).
char_in(incomengineer, star_wars4).
plays(shane_rimmer, incomengineer).
played_by(incomengineer, shane_rimmer).
proper_noun(pam_rose) --> [pam, rose].
actor(pam_rose).
is_a(pam_rose, actor).
acts_in(pam_rose, star_wars4).
proper_noun(leesubsirln) --> [leesubsirln].
character(leesubsirln).
is_a(leesubsirln, character).
char_in(leesubsirln, star_wars4).
plays(pam_rose, leesubsirln).
played_by(leesubsirln, pam_rose).
proper_noun(george_roubicek) --> [george, roubicek].
actor(george_roubicek).
is_a(george_roubicek, actor).
acts_in(george_roubicek, star_wars4).
proper_noun(cmdrpraji_imperial_officer_no_2_on_rebel_ship) --> [cmdrpraji, imperial, officer, no_2, on, rebel, ship].
character(cmdrpraji_imperial_officer_no_2_on_rebel_ship).
is_a(cmdrpraji_imperial_officer_no_2_on_rebel_ship, character).
char_in(cmdrpraji_imperial_officer_no_2_on_rebel_ship, star_wars4).
plays(george_roubicek, cmdrpraji_imperial_officer_no_2_on_rebel_ship).
played_by(cmdrpraji_imperial_officer_no_2_on_rebel_ship, george_roubicek).
proper_noun(erica_simmons) --> [erica, simmons].
actor(erica_simmons).
is_a(erica_simmons, actor).
acts_in(erica_simmons, star_wars4).
proper_noun(tawsskhaa) --> [tawsskhaa].
character(tawsskhaa).
is_a(tawsskhaa, character).
char_in(tawsskhaa, star_wars4).
plays(erica_simmons, tawsskhaa).
played_by(tawsskhaa, erica_simmons).
proper_noun(angela_staines) --> [angela, staines].
actor(angela_staines).
is_a(angela_staines, actor).
acts_in(angela_staines, star_wars4).
proper_noun(sennitonnika) --> [sennitonnika].
character(sennitonnika).
is_a(sennitonnika, character).
char_in(sennitonnika, star_wars4).
plays(angela_staines, sennitonnika).
played_by(sennitonnika, angela_staines).
proper_noun(george_stock) --> [george, stock].
actor(george_stock).
is_a(george_stock, actor).
acts_in(george_stock, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(george_stock, cantinapatron).
played_by(cantinapatron, george_stock).
proper_noun(roy_straite) --> [roy, straite].
actor(roy_straite).
is_a(roy_straite, actor).
acts_in(roy_straite, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(roy_straite, cantinapatron).
played_by(cantinapatron, roy_straite).
proper_noun(peter_sturgeon) --> [peter, sturgeon].
actor(peter_sturgeon).
is_a(peter_sturgeon, actor).
acts_in(peter_sturgeon, star_wars4).
proper_noun(saitorrkal_fas) --> [saitorrkal, fas].
character(saitorrkal_fas).
is_a(saitorrkal_fas, character).
char_in(saitorrkal_fas, star_wars4).
plays(peter_sturgeon, saitorrkal_fas).
played_by(saitorrkal_fas, peter_sturgeon).
proper_noun(peter_sumner) --> [peter, sumner].
actor(peter_sumner).
is_a(peter_sumner, actor).
acts_in(peter_sumner, star_wars4).
proper_noun(ltpol_treidum) --> [ltpol, treidum].
character(ltpol_treidum).
is_a(ltpol_treidum, character).
char_in(ltpol_treidum, star_wars4).
plays(peter_sumner, ltpol_treidum).
played_by(ltpol_treidum, peter_sumner).
proper_noun(john_sylla) --> [john, sylla].
actor(john_sylla).
is_a(john_sylla, actor).
acts_in(john_sylla, star_wars4).
proper_noun(cantinavoices) --> [cantinavoices].
character(cantinavoices).
is_a(cantinavoices, character).
char_in(cantinavoices, star_wars4).
plays(john_sylla, cantinavoices).
played_by(cantinavoices, john_sylla).
proper_noun(tom_sylla) --> [tom, sylla].
actor(tom_sylla).
is_a(tom_sylla, actor).
acts_in(tom_sylla, star_wars4).
proper_noun(massassioutpost_announcer) --> [massassioutpost, announcer].
character(massassioutpost_announcer).
is_a(massassioutpost_announcer, character).
char_in(massassioutpost_announcer, star_wars4).
plays(tom_sylla, massassioutpost_announcer).
played_by(massassioutpost_announcer, tom_sylla).
proper_noun(malcolm_tierney) --> [malcolm, tierney].
actor(malcolm_tierney).
is_a(malcolm_tierney, actor).
acts_in(malcolm_tierney, star_wars4).
proper_noun(ltshann_childsen) --> [ltshann, childsen].
character(ltshann_childsen).
is_a(ltshann_childsen, character).
char_in(ltshann_childsen, star_wars4).
plays(malcolm_tierney, ltshann_childsen).
played_by(ltshann_childsen, malcolm_tierney).
proper_noun(phil_tippett) --> [phil, tippett].
actor(phil_tippett).
is_a(phil_tippett, actor).
acts_in(phil_tippett, star_wars4).
proper_noun(cantinaalien) --> [cantinaalien].
character(cantinaalien).
is_a(cantinaalien, character).
char_in(cantinaalien, star_wars4).
plays(phil_tippett, cantinaalien).
played_by(cantinaalien, phil_tippett).
proper_noun(frances_alfred_basil_tomlin) --> [frances, alfred, basil, tomlin].
actor(frances_alfred_basil_tomlin).
is_a(frances_alfred_basil_tomlin, actor).
acts_in(frances_alfred_basil_tomlin, star_wars4).
proper_noun(boshek) --> [boshek].
character(boshek).
is_a(boshek, character).
char_in(boshek, star_wars4).
plays(frances_alfred_basil_tomlin, boshek).
played_by(boshek, frances_alfred_basil_tomlin).
proper_noun(burnell_tucker) --> [burnell, tucker].
actor(burnell_tucker).
is_a(burnell_tucker, actor).
acts_in(burnell_tucker, star_wars4).
proper_noun(delgoren) --> [delgoren].
character(delgoren).
is_a(delgoren, character).
char_in(delgoren, star_wars4).
plays(burnell_tucker, delgoren).
played_by(delgoren, burnell_tucker).
proper_noun(morgan_upton) --> [morgan, upton].
actor(morgan_upton).
is_a(morgan_upton, actor).
acts_in(morgan_upton, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(morgan_upton, stormtrooper).
played_by(stormtrooper, morgan_upton).
proper_noun(jerry_walter) --> [jerry, walter].
actor(jerry_walter).
is_a(jerry_walter, actor).
acts_in(jerry_walter, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(jerry_walter, stormtrooper).
played_by(stormtrooper, jerry_walter).
proper_noun(hal_wamsley) --> [hal, wamsley].
actor(hal_wamsley).
is_a(hal_wamsley, actor).
acts_in(hal_wamsley, star_wars4).
proper_noun(jawa) --> [jawa].
character(jawa).
is_a(jawa, character).
char_in(jawa, star_wars4).
plays(hal_wamsley, jawa).
played_by(jawa, hal_wamsley).
proper_noun(larry_ward) --> [larry, ward].
actor(larry_ward).
is_a(larry_ward, actor).
acts_in(larry_ward, star_wars4).
proper_noun(greedo) --> [greedo].
character(greedo).
is_a(greedo, character).
char_in(greedo, star_wars4).
plays(larry_ward, greedo).
played_by(greedo, larry_ward).
proper_noun(diana_sadley_way) --> [diana, sadley, way].
actor(diana_sadley_way).
is_a(diana_sadley_way, actor).
acts_in(diana_sadley_way, star_wars4).
proper_noun(thuku) --> [thuku].
character(thuku).
is_a(thuku, character).
char_in(thuku, star_wars4).
plays(diana_sadley_way, thuku).
played_by(thuku, diana_sadley_way).
proper_noun(harold_weed) --> [harold, weed].
actor(harold_weed).
is_a(harold_weed, actor).
acts_in(harold_weed, star_wars4).
proper_noun(ketwol) --> [ketwol].
character(ketwol).
is_a(ketwol, character).
char_in(ketwol, star_wars4).
plays(harold_weed, ketwol).
played_by(ketwol, harold_weed).
proper_noun(bill_weston) --> [bill, weston].
actor(bill_weston).
is_a(bill_weston, actor).
acts_in(bill_weston, star_wars4).
proper_noun(stormtrooper) --> [stormtrooper].
character(stormtrooper).
is_a(stormtrooper, character).
char_in(stormtrooper, star_wars4).
plays(bill_weston, stormtrooper).
played_by(stormtrooper, bill_weston).
proper_noun(steve_spaz_williams) --> [steve, spaz, williams].
actor(steve_spaz_williams).
is_a(steve_spaz_williams, actor).
acts_in(steve_spaz_williams, star_wars4).
proper_noun(moseisley_citizen_special_edition) --> [moseisley, citizen, special, edition].
character(moseisley_citizen_special_edition).
is_a(moseisley_citizen_special_edition, character).
char_in(moseisley_citizen_special_edition, star_wars4).
plays(steve_spaz_williams, moseisley_citizen_special_edition).
played_by(moseisley_citizen_special_edition, steve_spaz_williams).
proper_noun(fred_wood) --> [fred, wood].
actor(fred_wood).
is_a(fred_wood, actor).
acts_in(fred_wood, star_wars4).
proper_noun(cantinapatron) --> [cantinapatron].
character(cantinapatron).
is_a(cantinapatron, character).
char_in(cantinapatron, star_wars4).
plays(fred_wood, cantinapatron).
played_by(cantinapatron, fred_wood).
