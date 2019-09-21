include common.mk

SUITES := data cuda results

.PHONY: compile
compile: $(SUITES)

.PHONY: $(SUITES)
$(SUITES):
	$(MAKE) -C $@

.PHONY: clean
clean:
	for SUITE in $(SUITES); do $(MAKE) -C $$SUITE clean; done
