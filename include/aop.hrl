-record(pointcut, {
	event = call::spawn|register|link|unlink|unregister|send|'receive'|call,
	module = null::string(),
	function = null::string(),
	payload = null::list(),
	advice_types = null::list()
}).

