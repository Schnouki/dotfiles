all: lafayette.xkb_symbols lafayette-ergo.svg ergol-ergo.svg
all: lafayette.xkb_symbols lafayette-ergo.svg

%.xkb_symbols: %.toml
	kalamine build --out $@ $<

%.svg: %.toml
	kalamine build --out $@ $<

%-ergo.svg: %.svg
	sed 's/\(<svg .*\) class="[^"]*"/\1 class="ol50 ergo mixed"/' $< > $@

lafayette.toml:
	wget https://raw.githubusercontent.com/fabi1cazenave/qwerty-lafayette/refs/heads/main/layouts/lafayette.toml

ergol.toml:
	wget https://raw.githubusercontent.com/Nuclear-Squid/ergol/refs/heads/main/keymaps/fr/ergol.toml
