src = .
dist = dist

copy = *.json

background = background.js

removeSrc = $(1:$(src)/%=%)
makeAndLink = mkdir -p $(dist)/$(dir $1); ln -f $(src)/$1 $(dist)/$1;

$(dist) :  $(background) $(addprefix $(src)/,$(copy))
	$(call map,removeSrc makeAndLink,$?)
	ln -f $(background) $(dist)
	touch $(dist)

$(background) : $(shell find src -type f)
	spago bundle-app -t $@

clean :
	rm -r $(dist) $(background)

# $(call compose,f1 f2 f3,w) -> $(call f3,$(call f2,$(call f1,w)))
compose = $(if $1,$(call compose,$(wordlist 2,$(words $1),$1),$(call $1,$2)),$2)

# $(call map,f1 f2,w1 w2) -> $(call compose,f1 f2,w1) $(call compose,f1 f2,w2)
map = $(foreach a,$2,$(call compose,$1,$a))
