fmt:
	cd squared_away && gleam format
	cd squared_away_lang && gleam format

test:
	cd squared_away && gleam test 
	cd squared_away_lang && gleam test

review:
	cd squared_away_lang && gleam run -m birdie

dev:
	cd squared_away && gleam run -m lustre/dev start

clean:
	cd squared_away && gleam clean 
	cd squared_away_lang && gleam clean