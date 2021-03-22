use jeopardy;

# Get the top 10 categories
SELECT category, count(*) AS games
FROM board
GROUP BY category
ORDER BY games DESC
LIMIT 10;

# Get the top 10 double Jeopardy locations
SELECT subquery.rowcat, subquery.colcat, count(*) as count
FROM
(
	SELECT board.clueid, board.doublejeop, location.rowcat, location.colcat
	FROM location
	LEFT JOIN board
    ON board.clueid = location.board_clueid
    WHERE board.doublejeop = 1
) AS subquery
GROUP BY subquery.rowcat, subquery.colcat
ORDER BY count(*) DESC
LIMIT 10;

# What are the most common values of the first chosen clue in the game?
SELECT score, count(*) AS games
FROM board
WHERE chosen = 1
GROUP BY score
ORDER BY score
LIMIT 10;

#find Arthur Chu's contestant ID
select * from players
where firstname = 'Arthur' and lastname = 'Chu';

#8885 is normal jeopardy player id, and 9514 is tounament of champs id

#what about Ken Jennings
select * from players
where firstname = 'Ken' and lastname = 'Jennings';
#1 is normal jeop id
#661 2005 tounament of champs
#7206 IBM/Watson games
#9010 Battle of the Decades
#12547 2019 all stars
#13086 the greatest of all time games

#find James H's contestant ID
select * from players
where firstname = 'James' and lastname = 'Holzhauer';
#12600 is normal player id
#12983 2019 tournmanet of champs
#13087 greatest of all time 

#only include their non-tournament IDs?

#pull all contestants to perform word analysis on occupation or location
select * from players;

#pull all of Arthur Chu's games
select * from players_has_episode
where players_playerid IN (8885, 9514);

#pull all of his scores
select finalscore, ansRight, ansWrong, players.playerid, players.firstname
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
where playerid IN (8885, 9514);

#Top 5 players average & max scores for non-tournament games
select players.firstname, players.lastname, round(avg(finalscore),1) AS average, max(finalscore) as max_score, round(avg(ansRight)) as avg_correct, round(avg(ansWrong)) as avg_incorrect, sum(ansRight) as total_correct, sum(ansWrong) as total_incorrect, count(*) as total_games
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
group by playerid
order by total_correct desc
limit 10;

#who has all time top correct answers
select players.firstname, max(finalscore) as max_score, round(avg(ansRight)) as avg_correct, round(avg(ansWrong)) as avg_incorrect, sum(ansRight) as total_correct, sum(ansWrong) as total_incorrect
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
group by playerid
order by total_correct desc;

#Ken Jenning's scores
select finalscore, ansRight, ansWrong, players.playerid, players.firstname, synopsis.episode_gameid
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
where playerid IN (1, 661, 7206, 9010, 12547, 13086)
order by finalscore desc;

#Ken Jenning's average answers right and wrong per game
select avg(ansRight) as Avg_Right, avg(ansWrong) as Avg_Wrong, players.playerid, players.firstname
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
where playerid IN (1, 661, 7206, 9010, 12547, 13086)
order by finalscore desc;

#global average
select avg(ansRight) as Avg_Right, avg(ansWrong) as Avg_Wrong
from synopsis;

#double jeopardy per game
select count(doublejeop), episode_gameid
from board
where doublejeop = 1
group by episode_gameid;

#double jeopardy clue info
select clueid, roundnum, category, clue, answer, episode_gameid
from board
where doublejeop = 1;

#who got double jeopardy clue
select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
from board
INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
INNER JOIN players on players.playerid = doubles_has_scores.playerid
where doublejeop = 1;

#summarize totals by playerid, and show top 10 players
select count(board.clueid) as double_jeop_count, players.playerid, players.firstname, players.lastname
from board
INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
INNER JOIN players on players.playerid = doubles_has_scores.playerid
where doublejeop = 1
GROUP BY playerid
order by double_jeop_count desc
limit 10;



SELECT subquery.rowcat, subquery.colcat, count(*) as count_dj
  FROM
    (
      SELECT board.clueid, board.doublejeop, location.rowcat, location.colcat
      FROM location
      LEFT JOIN board
      ON board.clueid = location.board_clueid
      WHERE board.doublejeop = 1
    ) AS subquery
  GROUP BY subquery.rowcat, subquery.colcat
  ORDER BY count_dj DESC;
  
  
SELECT board.clueid, board.doublejeop, location.rowcat, location.colcat
FROM location
LEFT JOIN board
ON board.clueid = location.board_clueid
WHERE board.doublejeop = 1;

select count(board.clueid) as loc_count, players.playerid, players.firstname, players.lastname, location.rowcat, location.colcat
from board
INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
INNER JOIN location on board.clueid = location.board_clueid
INNER JOIN players on players.playerid = doubles_has_scores.playerid
where doublejeop = 1 and players.playerid in (1, 12600, 861, 12824, 9037, 10171, 11663, 8885, 10911, 8522)
group by playerid, rowcat, colcat
order by playerid;

#top locations overall between top ten players
select count(board.clueid) as loc_count, location.rowcat, location.colcat
from board
INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
INNER JOIN location on board.clueid = location.board_clueid
INNER JOIN players on players.playerid = doubles_has_scores.playerid
where doublejeop = 1 and players.playerid in (1, 12600, 861, 12824, 9037, 10171, 11663, 8885, 10911, 8522)
group by rowcat, colcat
order by loc_count desc;

#min & max daily double wager
select min(abs(double_score)) as minimum_wager, max(abs(double_score)) as max_wager
from doubles_has_scores;

#find most common daily double wager
select abs(double_score), count(*)
from doubles_has_scores
group by abs(double_score)
order by count(*) desc
limit 1;

#who has the top all time scores?
select finalscore, players.firstname, players.lastname
from synopsis
inner join synopsis_has_players on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
inner join players on players.playerid = synopsis_has_players.players_playerid
order by finalscore desc
limit 10;

#highest score for Ken Jennings
select finalscore, players.firstname, players.lastname
from synopsis
inner join synopsis_has_players on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
inner join players on players.playerid = synopsis_has_players.players_playerid
where players.firstname = 'Ken' and players.lastname = 'Jennings'
order by finalscore desc
limit 10;

#get all game player info to perform winner analysis in R
select finalscore, episode_gameid, finalscoreid, ansRight, ansWrong, players.firstname, players.lastname, players_playerid
from synopsis
inner join synopsis_has_players on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
inner join players on players.playerid = synopsis_has_players.players_playerid;




