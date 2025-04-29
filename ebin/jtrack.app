{application, 'jtrack', [
	{description, ""},
	{vsn, "rolling"},
	{modules, ['config_storage','config_storage_sup','issue_handler','issue_storage','issue_storage_sup','jtrack_app','jtrack_sup','tracker','tracker_sup','work_session_handler','work_session_storage','work_session_storage_sup']},
	{registered, [jtrack_sup]},
	{applications, [kernel,stdlib,cowboy,jiffy,hackney,uuid]},
	{optional_applications, []},
	{mod, {jtrack_app, []}},
	{env, []}
]}.