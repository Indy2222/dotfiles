#! /usr/bin/env python2
from subprocess import check_output

GMAIL_NAME_MAPPING = {
    '[Gmail]/Sent Mail': 'Sent',
    '[Gmail]/Drafts': 'Drafts'
}
GMAIL_NAME_MAPPING_REVERSE = {v: k for k, v in GMAIL_NAME_MAPPING.iteritems()}


_pwd_cache = {}


def get_pwd(name):
    global _pwd_cache

    try:
        return _pwd_cache[name]
    except KeyError:
        output = check_output(['pass', 'show', name])
        _pwd_cache[name] = output.splitlines()[0]
        return _pwd_cache[name]
