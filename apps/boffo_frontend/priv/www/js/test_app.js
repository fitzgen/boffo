define([
    "boffo"
], function(boffo) {
    var id_cache = {};
    var username;
    var chatlogs = '';

    function get_element(id) {
        var elm;

        if (!(id in id_cache)) {
            id_cache[id] = document.getElementById(id);
        }

        return id_cache[id];
    }

    function get_value(id) {
        return get_element(id).value;
    }

    function set_auth_class(cls) {
        get_element('auth-area').className = cls;
    }

    function handle_msg(evt) {
        chatlogs += evt.username + ': ' + evt.msg + '\n';
        get_element('chat').innerText = chatlogs;
    }

    function handle_key(e) {
        if (e.keyCode == 13) {
            boffo.send({msg: e.target.value});
            e.target.value = '';
        }
    }

    function init_app() {
        boffo.listen("msg", handle_msg);
        get_element("input").onkeypress = handle_key;
        get_element("main").className = 'active';
    }

    get_element('login-btn').onclick = function() {
        set_auth_class('loading');
        username = get_value('username');
        boffo.login(username, get_value('password'), function(ok) {
            var error;

            set_auth_class(ok ? 'logged-in' : 'logged-out');
            error = ok ? '' : 'Invalid credentials';
            get_element('auth-error').innerText = error;

            if (ok) {
                init_app();
            }
        });
    };

    get_element('logout-btn').onclick = function() {
        set_auth_class('logged-out');
        get_element('main').className = '';
        boffo.logout();
    }

    window.onbeforeunload = function() {
        if (username) {
            boffo.logout();
            username = null;
            //return "really????";
        }
    }

    window.boffo = boffo;
});
