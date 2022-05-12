return {
	settings = {
		gopls = {
			analyses = { unusedparams = true, unreachable = false },
			codelenses = {
				generate = true,
				gc_details = true,
				test = true,
				tidy = true,
			},
			usePlaceholders = true,
			completeUnimported = true,
			staticcheck = true,
			matcher = "fuzzy",
			diagnosticsDelay = "500ms",
			experimentalWatchedFileDelay = "1000ms",
			symbolMatcher = "fuzzy",
			gofumpt = false,
			buildFlags = { "-tags", "integration" },
		},
	},
}
