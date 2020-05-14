build:
	spago bundle-app
	spago test
	sass src/style.scss index.css
