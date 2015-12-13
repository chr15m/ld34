CSS=build/css/site.css
IDX=build/index.html
APP=build/js/app.js
XTRN=externs.js

all: $(APP) $(CSS) $(IDX)

$(CSS): resources/public/css/site.css
	mkdir -p build/css
	cp $< $@

$(APP): src/**/** $(XTRN) project.clj
	rm -f $(APP)
	lein clean
	lein cljsbuild once min

$(XTRN): src/cljs/**/** project.clj
	lein externs > $(XTRN)

$(IDX): src/clj/reagent_game_test/*.clj
	lein run -m reagent-game-test.utils/index-html > $(IDX)

clean:
	lein clean
	rm -rf $(CSS) $(APP) $(IDX) $(XTRN) build/css build/js
