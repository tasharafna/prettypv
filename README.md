# home solar export estimator
Exploring the implications of changes to solar net metering.

Many US states & power utilities are proposing changes to long-standing net metering programs. It's becoming much more important to match solar generation profiles with onsite power use. 

Home Solar Export Estimator  sums up hour-by-hour data from NREL to illuminate the tradeoff between *minimizing* solar exports to the grid & *maximizing* solar coverage of household electricity use.

Home Solar Export Estimator is a simple wrapper that combines two data sources: 1) Estimated hourly solar generation from NREL PVWatts at ~1000 U.S. locations based on PV system specs and irradidance from a 'typical meterological year', 2) Simulated hourly electricity use profiles (low, base, high) for each location based on generic household consumption patterns adjusted for local climate, also developed by NREL. Solar generation assumes 'standard' module type and 'fixed (roof-mount)' array type. Battery storage is assumed to have a 100% round-trip efficiency.
