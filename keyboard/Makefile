all: lafayette.xkb_symbols lafayette.svg

lafayette.xkb_symbols: lafayette.toml
	kalamine build --out $@ $<

lafayette.svg: lafayette.toml
	kalamine build --out $@ $<

lafayette.toml:
	wget https://raw.githubusercontent.com/fabi1cazenave/qwerty-lafayette/refs/heads/main/layouts/lafayette.toml
