build:
	sass src/style.scss index.css
	spago bundle-app
	spago test
