REBAR = rebar
DIALYZER = dialyzer

all: deps compile

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

compile-nodeps:
	$(REBAR) compile skip_deps=true

doc: compile
	$(REBAR) doc skip_deps=true

clean:
	$(REBAR) clean
	rm -rf ebin

distclean: clean
	rm -rf deps

# Dialyzer

APPS = kernel stdlib erts crypto public_key ssl compiler asn1 sasl

build-plt:
	$(DIALYZER) --build_plt \
		--output_plt .$(PROJECT).plt \
		--apps $(APPS)

plt-add-deps:
	$(DIALYZER) --add_to_plt --plt .$(PROJECT).plt \
		--output_plt .$(PROJECT).plt -r deps/

plt-remove-deps:
	$(DIALYZER) --remove_from_plt --plt .$(PROJECT).plt \
		--output_plt .$(PROJECT).plt -r deps/

plt-readd-deps: plt-remove-deps plt-add-deps

dialyze: compile-nodeps
	$(DIALYZER) --fullpath --plt .$(PROJECT).plt ebin \
		-Werror_handling -Wrace_conditions -Wunmatched_returns

.PHONY: all deps compile compile-nodeps doc clean distclean \
	build-plt plt-add-deps plt-remove-deps plt-readd-deps dialyzer
