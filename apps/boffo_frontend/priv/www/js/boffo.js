define([
    "xhr"
], function(xhr) {
    var TIMEOUT = 500,
        URLS = {
            login: '/api/login',
            logout: '/api/logout',
            feed: '/api/feed/',
            msg: '/api/msg'
        };

/*********************************************************************
 * globals */
    var listeners = {};
    var auth_token = null;
    var comet = null;

/*********************************************************************
 * Event listening / handling */
    function handle_evt(event) {
        var i, type;

        type = event.type;
        if (type in listeners) {
            for (i=0; i<listeners[type].length; i++) {
                listeners[type][i](event.data);
            }
        }
    }

    function handle_close() {
        console.log('-- xhr closed.');
    }

    function listen(evt_type, callback) {
        if (!(evt_type in listeners)) {
            listeners[evt_type] = [];
        }
        listeners[evt_type].push(callback);
    }


/*********************************************************************
 * Initialization */
    function login(username, password, callback) {
        params = {username: username, password: password};
        xhr.json_post(URLS.login, params, function(resp) {
            if (resp.error) {
                callback(false);
            } else {
                callback(true);
                auth_token = resp.token;
                if (comet) {
                    comet.close();
                }
                comet = new xhr.Comet(URLS.feed, {token: auth_token}, handle_evt, handle_close);
            }
        });
        window.xhr = xhr;
    }

    function logout() {
        if (auth_token) {
            console.log('logging out with auth token: ', auth_token);
        }

        if (comet) {
            comet.close();
        }

        xhr.json_post(URLS.logout, {token: auth_token}, null, true);

        auth_token = null;
        comet = null;
    }


/*********************************************************************
 * Other API */
    function send(msg) {
        var params;

        if (auth_token === null) {
            return false;
        }

        var params = {token: auth_token, msg: JSON.stringify(msg)};
        xhr.json_post(URLS.msg, params);

        return true;
    }


/*********************************************************************
 * Module export */
    return {
        login: login,
        logout: logout,
        send: send,
        listen: listen
    };
});

// rectangle kill C-x r k
