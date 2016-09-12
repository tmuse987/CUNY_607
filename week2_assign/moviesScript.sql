#This short script creates a movieRatings table in a movie_ratings database (aka schema)
# and then proceeds to populate with a sampling of movie ratings.

drop database if exists movie_ratings;
create database movie_ratings;
use movie_ratings;

create table movieRatings
( 
	seq int auto_increment key,
	movie varchar(100),
    rating int
);
	
    
insert into movieRatings values (0, 'Terminator', 5);
insert into movieRatings values (0, 'The Matrix', 3);
insert into movieRatings values (0, 'Duck Soup', 5);
insert into movieRatings values (0, 'Never Say Never', 1);
insert into movieRatings values (0, 'Babadook', 5);
insert into movieRatings values (0, 'Gone Girl', 4);

select * from movieratings
