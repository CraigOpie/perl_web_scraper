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
*/
proper_noun(mark_hamill) --> [mark, hamill].
actor(mark_hamill).
is_a(mark_hamill, actor).
acts_in(mark_hamill, relatively_super).
proper_noun(relatively_super) --> [relatively, super].
acts_in(mark_hamill, howard_lovecraft_and_the_kingdom_of_madness).
proper_noun(howard_lovecraft_and_the_kingdom_of_madness) --> [howard, lovecraft, and, the, kingdom, of, madness].
acts_in(mark_hamill, lego_dc_super_heroes_justice_league_attack_of_the_legion_of_doom).
proper_noun(lego_dc_super_heroes_justice_league_attack_of_the_legion_of_doom) --> [lego, dc, super, heroes, justice, league, attack, of, the, legion, of, doom].
acts_in(mark_hamill, raiders_raptors_and_rebels_behind_the_magic_of_ilm).
proper_noun(raiders_raptors_and_rebels_behind_the_magic_of_ilm) --> [raiders, raptors, and, rebels, behind, the, magic, of, ilm].
acts_in(mark_hamill, scooby-doo_moon_monster_madness).
proper_noun(scooby-doo_moon_monster_madness) --> [scooby-doo, moon, monster, madness].
acts_in(mark_hamill, metalocalypse_the_doomstar_requiem_a_klok_opera).
proper_noun(metalocalypse_the_doomstar_requiem_a_klok_opera) --> [metalocalypse, the, doomstar, requiem, a, klok, opera].
acts_in(mark_hamill, exchange_student_zero).
proper_noun(exchange_student_zero) --> [exchange, student, zero].
acts_in(mark_hamill, lego_hero_factory_rise_of_the_rookies).
proper_noun(lego_hero_factory_rise_of_the_rookies) --> [lego, hero, factory, rise, of, the, rookies].
acts_in(mark_hamill, scooby-doo_camp_scare).
proper_noun(scooby-doo_camp_scare) --> [scooby-doo, camp, scare].
acts_in(mark_hamill, super_duper_super_sleuths).
proper_noun(super_duper_super_sleuths) --> [super, duper, super, sleuths].
acts_in(mark_hamill, dantes_inferno_an_animated_epic).
proper_noun(dantes_inferno_an_animated_epic) --> [dantes, inferno, an, animated, epic].
acts_in(mark_hamill, tigger_pooh_and_a_musical_too).
proper_noun(tigger_pooh_and_a_musical_too) --> [tigger, pooh, and, a, musical, too].
acts_in(mark_hamill, afro_samurai_resurrection).
proper_noun(afro_samurai_resurrection) --> [afro, samurai, resurrection].
acts_in(mark_hamill, futurama_benders_big_score).
proper_noun(futurama_benders_big_score) --> [futurama, benders, big, score].
acts_in(mark_hamill, tom_and_jerry_in_shiver_me_whiskers).
proper_noun(tom_and_jerry_in_shiver_me_whiskers) --> [tom, and, jerry, in, shiver, me, whiskers].
acts_in(mark_hamill, codename_kids_next_door_operation_zero).
proper_noun(codename_kids_next_door_operation_zero) --> [codename, kids, next, door, operation, zero].
acts_in(mark_hamill, choose_your_own_adventure_the_abominable_snowman).
proper_noun(choose_your_own_adventure_the_abominable_snowman) --> [choose, your, own, adventure, the, abominable, snowman].
acts_in(mark_hamill, ultimate_avengers_ii).
proper_noun(ultimate_avengers_ii) --> [ultimate, avengers, ii].
acts_in(mark_hamill, queer_duck_the_movie).
proper_noun(queer_duck_the_movie) --> [queer, duck, the, movie].
acts_in(mark_hamill, comic_book_the_movie).
proper_noun(comic_book_the_movie) --> [comic, book, the, movie].
acts_in(mark_hamill, aero-troopers_the_nemeclous_crusade).
proper_noun(aero-troopers_the_nemeclous_crusade) --> [aero-troopers, the, nemeclous, crusade].
acts_in(mark_hamill, cartoon_networks_funniest_bloopers_and_other_embarrassing_moments).
proper_noun(cartoon_networks_funniest_bloopers_and_other_embarrassing_moments) --> [cartoon, networks, funniest, bloopers, and, other, embarrassing, moments].
acts_in(mark_hamill, rapsittie_street_kids_believe_in_santa).
proper_noun(rapsittie_street_kids_believe_in_santa) --> [rapsittie, street, kids, believe, in, santa].
acts_in(mark_hamill, balto_wolf_quest).
proper_noun(balto_wolf_quest) --> [balto, wolf, quest].
acts_in(mark_hamill, batman_beyond_return_of_the_joker).
proper_noun(batman_beyond_return_of_the_joker) --> [batman, beyond, return, of, the, joker].
acts_in(mark_hamill, joseph_king_of_dreams).
proper_noun(joseph_king_of_dreams) --> [joseph, king, of, dreams].
acts_in(mark_hamill, scooby-doo_and_the_alien_invaders).
proper_noun(scooby-doo_and_the_alien_invaders) --> [scooby-doo, and, the, alien, invaders].
acts_in(mark_hamill, the_christmas_lamb).
proper_noun(the_christmas_lamb) --> [the, christmas, lamb].
acts_in(mark_hamill, the_night_of_the_headless_horseman).
proper_noun(the_night_of_the_headless_horseman) --> [the, night, of, the, headless, horseman].
acts_in(mark_hamill, scooby-doo_on_zombie_island).
proper_noun(scooby-doo_on_zombie_island) --> [scooby-doo, on, zombie, island].
acts_in(mark_hamill, gen3438).
proper_noun(gen3438) --> [gen3438].
acts_in(mark_hamill, when_time_expires).
proper_noun(when_time_expires) --> [when, time, expires].
acts_in(mark_hamill, bruno_the_kid_the_animated_movie).
proper_noun(bruno_the_kid_the_animated_movie) --> [bruno, the, kid, the, animated, movie].
acts_in(mark_hamill, hollyrock-a-bye_baby).
proper_noun(hollyrock-a-bye_baby) --> [hollyrock-a-bye, baby].
acts_in(mark_hamill, body_bags).
proper_noun(body_bags) --> [body, bags].
acts_in(mark_hamill, damour_et_daventure_une_image_de_trop).
proper_noun(damour_et_daventure_une_image_de_trop) --> [damour, et, daventure, une, image, de, trop].
acts_in(mark_hamill, earth_angel).
proper_noun(earth_angel) --> [earth, angel].
acts_in(mark_hamill, the_star_wars_holiday_special).
proper_noun(the_star_wars_holiday_special) --> [the, star, wars, holiday, special].
acts_in(mark_hamill, the_city).
proper_noun(the_city) --> [the, city].
acts_in(mark_hamill, mallory_circumstantial_evidence).
proper_noun(mallory_circumstantial_evidence) --> [mallory, circumstantial, evidence].
acts_in(mark_hamill, eric).
proper_noun(eric) --> [eric].
acts_in(mark_hamill, delancey_street_the_crisis_within).
proper_noun(delancey_street_the_crisis_within) --> [delancey, street, the, crisis, within].
acts_in(mark_hamill, sarah_t_portrait_of_a_teenage_alcoholic).
proper_noun(sarah_t_portrait_of_a_teenage_alcoholic) --> [sarah, t, portrait, of, a, teenage, alcoholic].
acts_in(mark_hamill, comic_book_the_movie).
proper_noun(comic_book_the_movie) --> [comic, book, the, movie].
is_a(mark_hamill, director).
directed(mark_hamill, lego_star_wars_revenge_of_the_brick).
directed(mark_hamill, comic_book_the_movie).
acts_in(mark_hamill, comic_book_the_movie).
proper_noun(comic_book_the_movie) --> [comic, book, the, movie].
acts_in(mark_hamill, tigger_pooh_and_a_musical_too).
proper_noun(tigger_pooh_and_a_musical_too) --> [tigger, pooh, and, a, musical, too].
acts_in(mark_hamill, futurama_benders_big_score).
proper_noun(futurama_benders_big_score) --> [futurama, benders, big, score].
acts_in(mark_hamill, balto_wolf_quest).
proper_noun(balto_wolf_quest) --> [balto, wolf, quest].
acts_in(mark_hamill, comic_book_the_movie).
proper_noun(comic_book_the_movie) --> [comic, book, the, movie].
acts_in(mark_hamill, aero-troopers_the_nemeclous_crusade).
proper_noun(aero-troopers_the_nemeclous_crusade) --> [aero-troopers, the, nemeclous, crusade].
acts_in(mark_hamill, dear_class_of_2020).
proper_noun(dear_class_of_2020) --> [dear, class, of, 2020].
acts_in(mark_hamill, the_investigation_a_search_for_the_truth_in_ten_acts).
proper_noun(the_investigation_a_search_for_the_truth_in_ten_acts) --> [the, investigation, a, search, for, the, truth, in, ten, acts].
acts_in(mark_hamill, star_wars_the_force_awakens_world_premiere_red_carpet).
proper_noun(star_wars_the_force_awakens_world_premiere_red_carpet) --> [star, wars, the, force, awakens, world, premiere, red, carpet].
acts_in(mark_hamill, new_years_rotten_eve).
proper_noun(new_years_rotten_eve) --> [new, years, rotten, eve].
acts_in(mark_hamill, the_film_society_of_lincoln_center_annual_gala_tribute_to_alec_guinness).
proper_noun(the_film_society_of_lincoln_center_annual_gala_tribute_to_alec_guinness) --> [the, film, society, of, lincoln, center, annual, gala, tribute, to, alec, guinness].
acts_in(mark_hamill, the_great_space_coaster_supershow).
proper_noun(the_great_space_coaster_supershow) --> [the, great, space, coaster, supershow].
acts_in(mark_hamill, the_flash_ii_revenge_of_the_trickster).
proper_noun(the_flash_ii_revenge_of_the_trickster) --> [the, flash, ii, revenge, of, the, trickster].
proper_noun(mark_hamill) --> [mark, hamill].
actor(mark_hamill).
is_a(mark_hamill, actor).
acts_in(mark_hamill, star_wars4).
proper_noun(luke_skywalker) --> [luke, skywalker].
character(luke_skywalker).
is_a(luke_skywalker, character).
char_in(luke_skywalker, star_wars4).
plays(mark_hamill, luke_skywalker).
played_by(luke_skywalker, mark_hamill).
proper_noun(harrison_ford) --> [harrison, ford].
actor(harrison_ford).
is_a(harrison_ford, actor).
acts_in(harrison_ford, star_wars4).
proper_noun(han_solo) --> [han, solo].
character(han_solo).
is_a(han_solo, character).
char_in(han_solo, star_wars4).
plays(harrison_ford, han_solo).
played_by(han_solo, harrison_ford).
proper_noun(carrie_fisher) --> [carrie, fisher].
actor(carrie_fisher).
is_a(carrie_fisher, actor).
acts_in(carrie_fisher, star_wars4).
proper_noun(princess_leia_organa) --> [princess, leia, organa].
character(princess_leia_organa).
is_a(princess_leia_organa, character).
char_in(princess_leia_organa, star_wars4).
plays(carrie_fisher, princess_leia_organa).
played_by(princess_leia_organa, carrie_fisher).
proper_noun(peter_cushing) --> [peter, cushing].
actor(peter_cushing).
is_a(peter_cushing, actor).
acts_in(peter_cushing, star_wars4).
proper_noun(grand_moff_tarkin) --> [grand, moff, tarkin].
character(grand_moff_tarkin).
is_a(grand_moff_tarkin, character).
char_in(grand_moff_tarkin, star_wars4).
plays(peter_cushing, grand_moff_tarkin).
played_by(grand_moff_tarkin, peter_cushing).
proper_noun(alec_guinness) --> [alec, guinness].
actor(alec_guinness).
is_a(alec_guinness, actor).
acts_in(alec_guinness, star_wars4).
proper_noun(ben_obi-wan_kenobi) --> [ben, obi-wan, kenobi].
character(ben_obi-wan_kenobi).
is_a(ben_obi-wan_kenobi, character).
char_in(ben_obi-wan_kenobi, star_wars4).
plays(alec_guinness, ben_obi-wan_kenobi).
played_by(ben_obi-wan_kenobi, alec_guinness).
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
proper_noun(darth_vader) --> [darth, vader].
character(darth_vader).
is_a(darth_vader, character).
char_in(darth_vader, star_wars4).
plays(david_prowse, darth_vader).
played_by(darth_vader, david_prowse).
proper_noun(phil_brown) --> [phil, brown].
actor(phil_brown).
is_a(phil_brown, actor).
acts_in(phil_brown, star_wars4).
proper_noun(uncle_owen) --> [uncle, owen].
character(uncle_owen).
is_a(uncle_owen, character).
char_in(uncle_owen, star_wars4).
plays(phil_brown, uncle_owen).
played_by(uncle_owen, phil_brown).
proper_noun(shelagh_fraser) --> [shelagh, fraser].
actor(shelagh_fraser).
is_a(shelagh_fraser, actor).
acts_in(shelagh_fraser, star_wars4).
proper_noun(aunt_beru) --> [aunt, beru].
character(aunt_beru).
is_a(aunt_beru, character).
char_in(aunt_beru, star_wars4).
plays(shelagh_fraser, aunt_beru).
played_by(aunt_beru, shelagh_fraser).
proper_noun(jack_purvis) --> [jack, purvis].
actor(jack_purvis).
is_a(jack_purvis, actor).
acts_in(jack_purvis, star_wars4).
proper_noun(chief_jawa) --> [chief, jawa].
character(chief_jawa).
is_a(chief_jawa, character).
char_in(chief_jawa, star_wars4).
plays(jack_purvis, chief_jawa).
played_by(chief_jawa, jack_purvis).
proper_noun(alex_mccrindle) --> [alex, mccrindle].
actor(alex_mccrindle).
is_a(alex_mccrindle, actor).
acts_in(alex_mccrindle, star_wars4).
proper_noun(general_dodonna) --> [general, dodonna].
character(general_dodonna).
is_a(general_dodonna, character).
char_in(general_dodonna, star_wars4).
plays(alex_mccrindle, general_dodonna).
played_by(general_dodonna, alex_mccrindle).
proper_noun(eddie_byrne) --> [eddie, byrne].
actor(eddie_byrne).
is_a(eddie_byrne, actor).
acts_in(eddie_byrne, star_wars4).
proper_noun(general_willard) --> [general, willard].
character(general_willard).
is_a(general_willard, character).
char_in(general_willard, star_wars4).
plays(eddie_byrne, general_willard).
played_by(general_willard, eddie_byrne).
proper_noun(drewe_henley) --> [drewe, henley].
actor(drewe_henley).
is_a(drewe_henley, actor).
acts_in(drewe_henley, star_wars4).
proper_noun(red_leader) --> [red, leader].
character(red_leader).
is_a(red_leader, character).
char_in(red_leader, star_wars4).
plays(drewe_henley, red_leader).
played_by(red_leader, drewe_henley).
proper_noun(denis_lawson) --> [denis, lawson].
actor(denis_lawson).
is_a(denis_lawson, actor).
acts_in(denis_lawson, star_wars4).
proper_noun(red_two_wedge) --> [red, two, wedge].
character(red_two_wedge).
is_a(red_two_wedge, character).
char_in(red_two_wedge, star_wars4).
plays(denis_lawson, red_two_wedge).
played_by(red_two_wedge, denis_lawson).
proper_noun(garrick_hagon) --> [garrick, hagon].
actor(garrick_hagon).
is_a(garrick_hagon, actor).
acts_in(garrick_hagon, star_wars4).
proper_noun(red_three_biggs) --> [red, three, biggs].
character(red_three_biggs).
is_a(red_three_biggs, character).
char_in(red_three_biggs, star_wars4).
plays(garrick_hagon, red_three_biggs).
played_by(red_three_biggs, garrick_hagon).
proper_noun(jack_klaff) --> [jack, klaff].
actor(jack_klaff).
is_a(jack_klaff, actor).
acts_in(jack_klaff, star_wars4).
proper_noun(red_four_john_d) --> [red, four, john, d].
character(red_four_john_d).
is_a(red_four_john_d, character).
char_in(red_four_john_d, star_wars4).
plays(jack_klaff, red_four_john_d).
played_by(red_four_john_d, jack_klaff).
proper_noun(william_hootkins) --> [william, hootkins].
actor(william_hootkins).
is_a(william_hootkins, actor).
acts_in(william_hootkins, star_wars4).
proper_noun(red_six_porkins) --> [red, six, porkins].
character(red_six_porkins).
is_a(red_six_porkins, character).
char_in(red_six_porkins, star_wars4).
plays(william_hootkins, red_six_porkins).
played_by(red_six_porkins, william_hootkins).
proper_noun(angus_macinnes) --> [angus, macinnes].
actor(angus_macinnes).
is_a(angus_macinnes, actor).
acts_in(angus_macinnes, star_wars4).
proper_noun(gold_leader) --> [gold, leader].
character(gold_leader).
is_a(gold_leader, character).
char_in(gold_leader, star_wars4).
plays(angus_macinnes, gold_leader).
played_by(gold_leader, angus_macinnes).
proper_noun(jeremy_sinden) --> [jeremy, sinden].
actor(jeremy_sinden).
is_a(jeremy_sinden, actor).
acts_in(jeremy_sinden, star_wars4).
proper_noun(gold_two) --> [gold, two].
character(gold_two).
is_a(gold_two, character).
char_in(gold_two, star_wars4).
plays(jeremy_sinden, gold_two).
played_by(gold_two, jeremy_sinden).
proper_noun(graham_ashley) --> [graham, ashley].
actor(graham_ashley).
is_a(graham_ashley, actor).
acts_in(graham_ashley, star_wars4).
proper_noun(gold_five) --> [gold, five].
character(gold_five).
is_a(gold_five, character).
char_in(gold_five, star_wars4).
plays(graham_ashley, gold_five).
played_by(gold_five, graham_ashley).
proper_noun(don_henderson) --> [don, henderson].
actor(don_henderson).
is_a(don_henderson, actor).
acts_in(don_henderson, star_wars4).
proper_noun(general_taggi) --> [general, taggi].
character(general_taggi).
is_a(general_taggi, character).
char_in(general_taggi, star_wars4).
plays(don_henderson, general_taggi).
played_by(general_taggi, don_henderson).
proper_noun(richard_leparmentier) --> [richard, leparmentier].
actor(richard_leparmentier).
is_a(richard_leparmentier, actor).
acts_in(richard_leparmentier, star_wars4).
proper_noun(general_motti) --> [general, motti].
character(general_motti).
is_a(general_motti, character).
char_in(general_motti, star_wars4).
plays(richard_leparmentier, general_motti).
played_by(general_motti, richard_leparmentier).
proper_noun(leslie_schofield) --> [leslie, schofield].
actor(leslie_schofield).
is_a(leslie_schofield, actor).
acts_in(leslie_schofield, star_wars4).
proper_noun(commander_no_1) --> [commander, no_1].
character(commander_no_1).
is_a(commander_no_1, character).
char_in(commander_no_1, star_wars4).
plays(leslie_schofield, commander_no_1).
played_by(commander_no_1, leslie_schofield).
proper_noun(david_ankrum) --> [david, ankrum].
actor(david_ankrum).
is_a(david_ankrum, actor).
acts_in(david_ankrum, star_wars4).
proper_noun(red_two) --> [red, two].
character(red_two).
is_a(red_two, character).
char_in(red_two, star_wars4).
plays(david_ankrum, red_two).
played_by(red_two, david_ankrum).
proper_noun(mark_anthony_austin) --> [mark, anthony, austin].
actor(mark_anthony_austin).
is_a(mark_anthony_austin, actor).
acts_in(mark_anthony_austin, star_wars4).
proper_noun(boba_fett_special_edition) --> [boba, fett, special, edition].
character(boba_fett_special_edition).
is_a(boba_fett_special_edition, character).
char_in(boba_fett_special_edition, star_wars4).
plays(mark_anthony_austin, boba_fett_special_edition).
played_by(boba_fett_special_edition, mark_anthony_austin).
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
proper_noun(cantina_alien) --> [cantina, alien].
character(cantina_alien).
is_a(cantina_alien, character).
char_in(cantina_alien, star_wars4).
plays(jon_berg, cantina_alien).
played_by(cantina_alien, jon_berg).
proper_noun(doug_beswick) --> [doug, beswick].
actor(doug_beswick).
is_a(doug_beswick, actor).
acts_in(doug_beswick, star_wars4).
proper_noun(cantina_alien) --> [cantina, alien].
character(cantina_alien).
is_a(cantina_alien, character).
char_in(cantina_alien, star_wars4).
plays(doug_beswick, cantina_alien).
played_by(cantina_alien, doug_beswick).
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
proper_noun(nabrun_leids) --> [nabrun, leids].
character(nabrun_leids).
is_a(nabrun_leids, character).
char_in(nabrun_leids, star_wars4).
plays(janice_burchette, nabrun_leids).
played_by(nabrun_leids, janice_burchette).
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
proper_noun(drifter_red_12) --> [drifter, red, 12].
character(drifter_red_12).
is_a(drifter_red_12, character).
char_in(drifter_red_12, star_wars4).
plays(john_chapman, drifter_red_12).
played_by(drifter_red_12, john_chapman).
proper_noun(gilda_cohen) --> [gilda, cohen].
actor(gilda_cohen).
is_a(gilda_cohen, actor).
acts_in(gilda_cohen, star_wars4).
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(gilda_cohen, cantina_patron).
played_by(cantina_patron, gilda_cohen).
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
proper_noun(dr_evazan) --> [dr, evazan].
character(dr_evazan).
is_a(dr_evazan, character).
char_in(dr_evazan, star_wars4).
plays(alfie_curtis, dr_evazan).
played_by(dr_evazan, alfie_curtis).
proper_noun(robert_davies) --> [robert, davies].
actor(robert_davies).
is_a(robert_davies, actor).
acts_in(robert_davies, star_wars4).
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(robert_davies, cantina_patron).
played_by(cantina_patron, robert_davies).
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
proper_noun(hrchek_kal_fas) --> [hrchek, kal, fas].
character(hrchek_kal_fas).
is_a(hrchek_kal_fas, character).
char_in(hrchek_kal_fas, star_wars4).
plays(barbie_denham, hrchek_kal_fas).
played_by(hrchek_kal_fas, barbie_denham).
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
proper_noun(djas_puhr) --> [djas, puhr].
character(djas_puhr).
is_a(djas_puhr, character).
char_in(djas_puhr, star_wars4).
plays(kim_falkinburg, djas_puhr).
played_by(djas_puhr, kim_falkinburg).
proper_noun(harry_fielder) --> [harry, fielder].
actor(harry_fielder).
is_a(harry_fielder, actor).
acts_in(harry_fielder, star_wars4).
proper_noun(death_star_trooper) --> [death, star, trooper].
character(death_star_trooper).
is_a(death_star_trooper, character).
char_in(death_star_trooper, star_wars4).
plays(harry_fielder, death_star_trooper).
played_by(death_star_trooper, harry_fielder).
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
proper_noun(stormtrooper_with_binoculars) --> [stormtrooper, with, binoculars].
character(stormtrooper_with_binoculars).
is_a(stormtrooper_with_binoculars, character).
char_in(stormtrooper_with_binoculars, star_wars4).
plays(ted_gagliano, stormtrooper_with_binoculars).
played_by(stormtrooper_with_binoculars, ted_gagliano).
proper_noun(salo_gardner) --> [salo, gardner].
actor(salo_gardner).
is_a(salo_gardner, actor).
acts_in(salo_gardner, star_wars4).
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(salo_gardner, cantina_patron).
played_by(cantina_patron, salo_gardner).
proper_noun(steve_gawley) --> [steve, gawley].
actor(steve_gawley).
is_a(steve_gawley, actor).
acts_in(steve_gawley, star_wars4).
proper_noun(death_star_trooper) --> [death, star, trooper].
character(death_star_trooper).
is_a(death_star_trooper, character).
char_in(death_star_trooper, star_wars4).
plays(steve_gawley, death_star_trooper).
played_by(death_star_trooper, steve_gawley).
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
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(isaac_grand, cantina_patron).
played_by(cantina_patron, isaac_grand).
proper_noun(nelson_hall) --> [nelson, hall].
actor(nelson_hall).
is_a(nelson_hall, actor).
acts_in(nelson_hall, star_wars4).
proper_noun(stormtrooper_special_edition) --> [stormtrooper, special, edition].
character(stormtrooper_special_edition).
is_a(stormtrooper_special_edition, character).
char_in(stormtrooper_special_edition, star_wars4).
plays(nelson_hall, stormtrooper_special_edition).
played_by(stormtrooper_special_edition, nelson_hall).
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
proper_noun(leias_rebel_escort) --> [leias, rebel, escort].
character(leias_rebel_escort).
is_a(leias_rebel_escort, character).
char_in(leias_rebel_escort, star_wars4).
plays(alan_harris, leias_rebel_escort).
played_by(leias_rebel_escort, alan_harris).
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
proper_noun(brea_tonnika) --> [brea, tonnika].
character(brea_tonnika).
is_a(brea_tonnika, character).
char_in(brea_tonnika, star_wars4).
plays(christine_hewett, brea_tonnika).
played_by(brea_tonnika, christine_hewett).
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
proper_noun(ponda_baba) --> [ponda, baba].
character(ponda_baba).
is_a(ponda_baba, character).
char_in(ponda_baba, star_wars4).
plays(tommy_ilsley, ponda_baba).
played_by(ponda_baba, tommy_ilsley).
proper_noun(joe_johnston) --> [joe, johnston].
actor(joe_johnston).
is_a(joe_johnston, actor).
acts_in(joe_johnston, star_wars4).
proper_noun(death_star_trooper) --> [death, star, trooper].
character(death_star_trooper).
is_a(death_star_trooper, character).
char_in(death_star_trooper, star_wars4).
plays(joe_johnston, death_star_trooper).
played_by(death_star_trooper, joe_johnston).
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
proper_noun(darth_vader) --> [darth, vader].
character(darth_vader).
is_a(darth_vader, character).
char_in(darth_vader, star_wars4).
plays(james_earl_jones, darth_vader).
played_by(darth_vader, james_earl_jones).
proper_noun(linda_jones) --> [linda, jones].
actor(linda_jones).
is_a(linda_jones, actor).
acts_in(linda_jones, star_wars4).
proper_noun(chall_bekan) --> [chall, bekan].
character(chall_bekan).
is_a(chall_bekan, character).
char_in(chall_bekan, star_wars4).
plays(linda_jones, chall_bekan).
played_by(chall_bekan, linda_jones).
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
proper_noun(daine_jir) --> [daine, jir].
character(daine_jir).
is_a(daine_jir, character).
char_in(daine_jir, star_wars4).
plays(al_lampert, daine_jir).
played_by(daine_jir, al_lampert).
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
proper_noun(temple_guard) --> [temple, guard].
character(temple_guard).
is_a(temple_guard, character).
char_in(temple_guard, star_wars4).
plays(derek_lyons, temple_guard).
played_by(temple_guard, derek_lyons).
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
proper_noun(stormtrooper_special_edition) --> [stormtrooper, special, edition].
character(stormtrooper_special_edition).
is_a(stormtrooper_special_edition, character).
char_in(stormtrooper_special_edition, star_wars4).
plays(rick_mccallum, stormtrooper_special_edition).
played_by(stormtrooper_special_edition, rick_mccallum).
proper_noun(grant_mccune) --> [grant, mccune].
actor(grant_mccune).
is_a(grant_mccune, actor).
acts_in(grant_mccune, star_wars4).
proper_noun(death_star_gunner) --> [death, star, gunner].
character(death_star_gunner).
is_a(death_star_gunner, character).
char_in(death_star_gunner, star_wars4).
plays(grant_mccune, death_star_gunner).
played_by(death_star_gunner, grant_mccune).
proper_noun(geoffrey_moon) --> [geoffrey, moon].
actor(geoffrey_moon).
is_a(geoffrey_moon, actor).
acts_in(geoffrey_moon, star_wars4).
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(geoffrey_moon, cantina_patron).
played_by(cantina_patron, geoffrey_moon).
proper_noun(mandy_morton) --> [mandy, morton].
actor(mandy_morton).
is_a(mandy_morton, actor).
acts_in(mandy_morton, star_wars4).
proper_noun(swilla_corey) --> [swilla, corey].
character(swilla_corey).
is_a(swilla_corey, character).
char_in(swilla_corey, star_wars4).
plays(mandy_morton, swilla_corey).
played_by(swilla_corey, mandy_morton).
proper_noun(lorne_peterson) --> [lorne, peterson].
actor(lorne_peterson).
is_a(lorne_peterson, actor).
acts_in(lorne_peterson, star_wars4).
proper_noun(massassi_base_rebel_scout) --> [massassi, base, rebel, scout].
character(massassi_base_rebel_scout).
is_a(massassi_base_rebel_scout, character).
char_in(massassi_base_rebel_scout, star_wars4).
plays(lorne_peterson, massassi_base_rebel_scout).
played_by(massassi_base_rebel_scout, lorne_peterson).
proper_noun(marcus_powell) --> [marcus, powell].
actor(marcus_powell).
is_a(marcus_powell, actor).
acts_in(marcus_powell, star_wars4).
proper_noun(rycar_ryjerd) --> [rycar, ryjerd].
character(rycar_ryjerd).
is_a(rycar_ryjerd, character).
char_in(rycar_ryjerd, star_wars4).
plays(marcus_powell, rycar_ryjerd).
played_by(rycar_ryjerd, marcus_powell).
proper_noun(shane_rimmer) --> [shane, rimmer].
actor(shane_rimmer).
is_a(shane_rimmer, actor).
acts_in(shane_rimmer, star_wars4).
proper_noun(incom_engineer) --> [incom, engineer].
character(incom_engineer).
is_a(incom_engineer, character).
char_in(incom_engineer, star_wars4).
plays(shane_rimmer, incom_engineer).
played_by(incom_engineer, shane_rimmer).
proper_noun(pam_rose) --> [pam, rose].
actor(pam_rose).
is_a(pam_rose, actor).
acts_in(pam_rose, star_wars4).
proper_noun(leesub_sirln) --> [leesub, sirln].
character(leesub_sirln).
is_a(leesub_sirln, character).
char_in(leesub_sirln, star_wars4).
plays(pam_rose, leesub_sirln).
played_by(leesub_sirln, pam_rose).
proper_noun(george_roubicek) --> [george, roubicek].
actor(george_roubicek).
is_a(george_roubicek, actor).
acts_in(george_roubicek, star_wars4).
proper_noun(cmdr_praji_imperial_officer_no_2_on_rebel_ship) --> [cmdr, praji, imperial, officer, no_2, on, rebel, ship].
character(cmdr_praji_imperial_officer_no_2_on_rebel_ship).
is_a(cmdr_praji_imperial_officer_no_2_on_rebel_ship, character).
char_in(cmdr_praji_imperial_officer_no_2_on_rebel_ship, star_wars4).
plays(george_roubicek, cmdr_praji_imperial_officer_no_2_on_rebel_ship).
played_by(cmdr_praji_imperial_officer_no_2_on_rebel_ship, george_roubicek).
proper_noun(erica_simmons) --> [erica, simmons].
actor(erica_simmons).
is_a(erica_simmons, actor).
acts_in(erica_simmons, star_wars4).
proper_noun(tawss_khaa) --> [tawss, khaa].
character(tawss_khaa).
is_a(tawss_khaa, character).
char_in(tawss_khaa, star_wars4).
plays(erica_simmons, tawss_khaa).
played_by(tawss_khaa, erica_simmons).
proper_noun(angela_staines) --> [angela, staines].
actor(angela_staines).
is_a(angela_staines, actor).
acts_in(angela_staines, star_wars4).
proper_noun(senni_tonnika) --> [senni, tonnika].
character(senni_tonnika).
is_a(senni_tonnika, character).
char_in(senni_tonnika, star_wars4).
plays(angela_staines, senni_tonnika).
played_by(senni_tonnika, angela_staines).
proper_noun(george_stock) --> [george, stock].
actor(george_stock).
is_a(george_stock, actor).
acts_in(george_stock, star_wars4).
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(george_stock, cantina_patron).
played_by(cantina_patron, george_stock).
proper_noun(roy_straite) --> [roy, straite].
actor(roy_straite).
is_a(roy_straite, actor).
acts_in(roy_straite, star_wars4).
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(roy_straite, cantina_patron).
played_by(cantina_patron, roy_straite).
proper_noun(peter_sturgeon) --> [peter, sturgeon].
actor(peter_sturgeon).
is_a(peter_sturgeon, actor).
acts_in(peter_sturgeon, star_wars4).
proper_noun(saitorr_kal_fas) --> [saitorr, kal, fas].
character(saitorr_kal_fas).
is_a(saitorr_kal_fas, character).
char_in(saitorr_kal_fas, star_wars4).
plays(peter_sturgeon, saitorr_kal_fas).
played_by(saitorr_kal_fas, peter_sturgeon).
proper_noun(peter_sumner) --> [peter, sumner].
actor(peter_sumner).
is_a(peter_sumner, actor).
acts_in(peter_sumner, star_wars4).
proper_noun(lt_pol_treidum) --> [lt, pol, treidum].
character(lt_pol_treidum).
is_a(lt_pol_treidum, character).
char_in(lt_pol_treidum, star_wars4).
plays(peter_sumner, lt_pol_treidum).
played_by(lt_pol_treidum, peter_sumner).
proper_noun(john_sylla) --> [john, sylla].
actor(john_sylla).
is_a(john_sylla, actor).
acts_in(john_sylla, star_wars4).
proper_noun(cantina_voices) --> [cantina, voices].
character(cantina_voices).
is_a(cantina_voices, character).
char_in(cantina_voices, star_wars4).
plays(john_sylla, cantina_voices).
played_by(cantina_voices, john_sylla).
proper_noun(tom_sylla) --> [tom, sylla].
actor(tom_sylla).
is_a(tom_sylla, actor).
acts_in(tom_sylla, star_wars4).
proper_noun(massassi_outpost_announcer) --> [massassi, outpost, announcer].
character(massassi_outpost_announcer).
is_a(massassi_outpost_announcer, character).
char_in(massassi_outpost_announcer, star_wars4).
plays(tom_sylla, massassi_outpost_announcer).
played_by(massassi_outpost_announcer, tom_sylla).
proper_noun(malcolm_tierney) --> [malcolm, tierney].
actor(malcolm_tierney).
is_a(malcolm_tierney, actor).
acts_in(malcolm_tierney, star_wars4).
proper_noun(lt_shann_childsen) --> [lt, shann, childsen].
character(lt_shann_childsen).
is_a(lt_shann_childsen, character).
char_in(lt_shann_childsen, star_wars4).
plays(malcolm_tierney, lt_shann_childsen).
played_by(lt_shann_childsen, malcolm_tierney).
proper_noun(phil_tippett) --> [phil, tippett].
actor(phil_tippett).
is_a(phil_tippett, actor).
acts_in(phil_tippett, star_wars4).
proper_noun(cantina_alien) --> [cantina, alien].
character(cantina_alien).
is_a(cantina_alien, character).
char_in(cantina_alien, star_wars4).
plays(phil_tippett, cantina_alien).
played_by(cantina_alien, phil_tippett).
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
proper_noun(del_goren) --> [del, goren].
character(del_goren).
is_a(del_goren, character).
char_in(del_goren, star_wars4).
plays(burnell_tucker, del_goren).
played_by(del_goren, burnell_tucker).
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
proper_noun(mos_eisley_citizen_special_edition) --> [mos, eisley, citizen, special, edition].
character(mos_eisley_citizen_special_edition).
is_a(mos_eisley_citizen_special_edition, character).
char_in(mos_eisley_citizen_special_edition, star_wars4).
plays(steve_spaz_williams, mos_eisley_citizen_special_edition).
played_by(mos_eisley_citizen_special_edition, steve_spaz_williams).
proper_noun(fred_wood) --> [fred, wood].
actor(fred_wood).
is_a(fred_wood, actor).
acts_in(fred_wood, star_wars4).
proper_noun(cantina_patron) --> [cantina, patron].
character(cantina_patron).
is_a(cantina_patron, character).
char_in(cantina_patron, star_wars4).
plays(fred_wood, cantina_patron).
played_by(cantina_patron, fred_wood).
