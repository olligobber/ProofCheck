build:
	elm-make src/main.elm --output=build/main.js
	sass src/style.scss build/style.css
