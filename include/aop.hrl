-record(pointcut, {
	event = call::spawn|register|link|unlink|unregister|send|'receive'|call,
	module = []::string(),
	function = []::string(),
	arity = 0::integer(),
	advice_types = []::list()
}).

