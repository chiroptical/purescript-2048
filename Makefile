watch:
	spago bundle-app --main Main --to assets/app.js --watch

clean:
	rm -rf .cache .spago node_modules .psci_modules output dist

serve:
	parcel serve assets/index.html -o index--parcelified.html

format:
	find src/ -name "*.purs" -exec purty format {} --write \;
