.PHONY:
ghcid:
	@ghcid -c 'stack ghci'

.PHONY:
ghcid-test:
	@ghcid -c 'stack ghci poda:lib poda:test:poda-test --ghci-options=-fobject-code' -T main
