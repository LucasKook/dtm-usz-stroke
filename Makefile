
SR = Rscript --no-restore --no-save

dependencies:
	$(SR) dependencies.R

full-repro: dependencies
	$(SR) code/run/run-ensembles.R
	$(SR) code/run/gam-polr.R
	$(SR) code/run/sample-size-extrapolation.R
	make partial-repro

partial-repro: dependencies
	$(SR) code/analysis/cal-binary.R
	$(SR) code/analysis/cal-ordinal.R
	$(SR) code/analysis/ci-binary.R
	$(SR) code/analysis/ci-ordinal.R
	$(SR) code/analysis/perf.R
	# $(SR) code/analysis/merge-results.R
	make figure-repro

figure-repro: dependencies
	# $(SR) code/visualize/distr-predictors.R
	$(SR) code/visualize/gam-polr.R
	$(SR) code/visualize/lor.R
	$(SR) code/visualize/perf-calpl.R
	$(SR) code/visualize/sample-size-extrapolation.R

clean:
	rm -rf results Rplots.pdf figures
