ci: npm-deps purs-deps bundle parcel-build format
	spago test

npm-deps:
	npm install

purs-deps:
	spago2nix install

parcel-build:
	./node_modules/.bin/parcel build assets/index.html

bundle:
	spago bundle-app --main Main --to assets/index.js

watch:
	spago bundle-app --main Main --to assets/index.js --watch

clean:
	rm -rf .cache .spago node_modules .psci_modules output dist

serve:
	./node_modules/.bin/parcel serve assets/index.html -o index--parcelified.html

format:
	find src/ test/ -name "*.purs" -exec purty {} --write \;

test:
	spago test
