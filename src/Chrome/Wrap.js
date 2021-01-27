exports.wrapApiImpl = Right2 => LeftPermissions => api => method => args => error => (onError, onSuccess) => {
	try {
		if (api in chrome) {
			const api_ = chrome[api];

			api_[method].apply(
				api_,
				[
					...args,
					a => {
						// some functions (e.g. chrome.bookmarks.getSubTree) throw an error asynchronously with seemingly no way to catch it.
						// When this happens, `a` seems to be undefined, so this is the best way I can think of to catch such errors.
						if (a === undefined)
							onSuccess(error);
						else
							onSuccess(Right2(a))
					}
				]
			);
		} else {
			onSuccess(LeftPermissions);
		}
	} catch (_) {
		onSuccess(error);
	}

	return (_, __, onCancelerSuccess) => onCancelerSuccess();
};

exports.wrapListenerImpl = Right => LeftPermissions => api => method => (onError, onSuccess) => {
	try {
		if (api in chrome) {
			const event = chrome[api][method];

			const listener = (...args) => {
				onSuccess(Right(args));
				event.removeListener(listener);
			}

			event.addListener(listener);

			return (_, __, onCancelerSuccess) => {
				event.removeListener(listener);
				onCancelerSuccess();
			}
		} else {
			onSuccess(LeftPermissions);
		}
	} catch (e) {
		onError(e);
	}

	return (_, __, onCancelerSuccess) => onCancelerSuccess();
}
