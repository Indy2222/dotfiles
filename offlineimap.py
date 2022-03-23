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

MGN_INCLUDE = {
    'Drafts',
    'INBOX',
    'Sent',
}
