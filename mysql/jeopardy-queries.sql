# Get the top 10 categories
SELECT category, count(*) AS games
FROM board
GROUP BY category
ORDER BY games DESC
LIMIT 10;

# Get the top 10 double Jeopardy locations
SELECT subquery.rowcat, subquery.colcat, count(*)
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

# Find Arthur Chu's contestant ID
select * from players
where firstname = 'Arthur' and lastname = 'Chu';

# Arthur Chu has two player ids! 8885 and 9514 - uh oh
# What about Ken Jennings?
select * from players
where firstname = 'Ken' and lastname = 'Jennings';

# Pull all of Arthur Chu's games
select * from players_has_episode
where players_playerid IN (8885, 9514);

# Pull all of his scores
select finalscore, ansRight, ansWrong, players.playerid, players.firstname
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
where playerid IN (8885, 9514);

# Arthur Chu's average & max scores
select avg(finalscore) AS average, max(finalscore) as max_score
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
where playerid IN (8885, 9514);

# Ken Jenning's scores
select finalscore, ansRight, ansWrong, players.playerid, players.firstname, synopsis.episode_gameid
from synopsis_has_players
INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
where playerid IN (1, 661, 7206, 9010, 12547, 13086)
order by finalscore desc;

# Double jeopardy per game
select count(doublejeop), episode_gameid
from board
where doublejeop = 1
group by episode_gameid;

# Who got the double jeopardy clue
select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
from board
INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
INNER JOIN players on players.playerid = doubles_has_scores.playerid
where doublejeop = 1;

# Summarize totals by playerid, and show top 10 players
select count(board.clueid) as double_jeop_count, players.playerid, players.firstname, players.lastname
from board
INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
INNER JOIN players on players.playerid = doubles_has_scores.playerid
where doublejeop = 1
GROUP BY playerid
order by double_jeop_count desc
limit 10;