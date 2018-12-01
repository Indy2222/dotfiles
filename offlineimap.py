#! /usr/bin/env python2
from subprocess import check_output

GMAIL_NAME_MAPPING = {
    '[Gmail]/Sent Mail': 'Sent',
    '[Gmail]/Drafts': 'Drafts'
}
GMAIL_NAME_MAPPING_REVERSE = {v: k for k, v in GMAIL_NAME_MAPPING.iteritems()}


def get_pwd(name):
    output = check_output(['pass', 'show', name])
    return output.splitlines()[0]
