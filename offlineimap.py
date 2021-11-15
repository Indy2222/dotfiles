#! /usr/bin/env python3
from subprocess import check_output

GMAIL_EXCLUDE = {
    '[Gmail]',
    '[Gmail]/All Mail',
    '[Gmail]/Important',
    '[Gmail]/Spam',
    '[Gmail]/Starred',
    '[Gmail]/Bin',
}

GMAIL_NAME_MAPPING = {
    '[Gmail]/Sent Mail': 'Sent',
    '[Gmail]/Drafts': 'Drafts',
    '[Gmail]/Trash': 'Trash',
    '[Gmail]/Subscriptions': 'Subscriptions',
}
GMAIL_NAME_MAPPING_REVERSE = {v: k for k, v in GMAIL_NAME_MAPPING.items()}


_pwd_cache = {}


def get_pwd(name):
    global _pwd_cache

    try:
        return _pwd_cache[name]
    except KeyError:
        output = check_output(['pass', 'show', name])
        _pwd_cache[name] = output.splitlines()[0]
        return _pwd_cache[name]
