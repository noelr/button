all:
	elm-make src/Main.elm --output=static/main.js
	stack build
	stack exec button
