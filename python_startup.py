# -*- coding: utf-8 -*-

# Source: http://sametmax.com/nouveau-fichier-de-start-up-python/

# faut que ça marche pareil en P2 et P3
#from __future__ import unicode_literals, print_function, absolute_import
from __future__ import print_function, absolute_import

# Les imports des modules de la libs standars que j'utilise le plus
# car à force çe me gave de les réimporter moi-même à chaque session
import sys
import os
import re
import json
import csv
import random
import hashlib
import tempfile
import random
import shelve
import atexit
import subprocess
from glob import glob
from uuid import uuid4
from pprint import pprint

# on shadow le open() builtin histoire d'avoir toujours le
# paramètre encoding
from codecs import open

from itertools import *
from collections import *
from datetime import datetime, timedelta

# imports d'outils tierces parties que j'utilise souvent mais qui pourraient ne
# pas être installés
try:
    import arrow
except ImportError:
    pass

try:
    import requests
except ImportError:
    pass

try:
    from path import path
except ImportError:
    pass

try:
    from minibelt import *
except ImportError:
    pass

# activation d'autocompletion si ce n'est pas déjà le cas, notamment sous
# des vieux shell Python ordinnaire
try:
    import rlcompleter
    import readline
    readline.parse_and_bind("tab: complete")
except ImportError:
    pass

# si on est dans un virtual env
env = os.environ.get('VIRTUAL_ENV')
if env:

    # afficher le nom de l'env dans le prompt (marche pas dans ipython qui
    # a sa propre config pour ça)
    env_name = os.path.basename(env)
    sys.ps1 = '(%s) %s ' % (env_name, getattr(sys, 'ps1', '>>>'))

    # affichage une fois des modules installés avec pip pour qu'on sache
    # ce qu'on a a dispo dans cet env
    def show_venv():
        print("\nVirtualenv '{}' contains:\n".format(env_name))
        cmd = subprocess.check_output([env + "/bin/pip", "freeze"],
                                      stderr=subprocess.STDOUT)
        try:
            cmd = cmd.decode('utf8')
        except:
            pass

        cmd = cmd.strip().split("\n")
        p = re.compile(r'(^.*\:\s)|((#|@).*$)|(==.*$)')
        print("'" + "', '".join(sorted(set(os.path.basename(p.sub('', f))
                                           for f in cmd))) + "'\n")


# alias pour printer rapidement
p = print
pp = pprint

# avoir toujours un dossier temporaire près à l'usage
TEMP_DIR = os.path.join(tempfile.gettempdir(), 'pythontemp')
try:
    os.makedirs(TEMP_DIR)
    TEMP_DIR = path(TEMP_DIR)  # si possible un objet path
except Exception as e:
    pass

# avoir un dico persistant pour garder des objets entre deux sessions. Pratique quand
# on a un gros array numpy qu'on n'a pas envie de se faire chier à se recréer


class Store(object):
    def __init__(self, filename):
        object.__setattr__(self, 'DICT', shelve.DbfilenameShelf(filename))
        # cleaning the dict on the way out
        atexit.register(self._clean)

    def __getattribute__(self, name):
        if name not in ("DICT", '_clean'):
            try:
                return self.DICT[name]
            except:
                return None
        return object.__getattribute__(self, name)

    def __setattr__(self, name, value):
        if name in ("DICT", '_clean'):
            raise ValueError("'%s' is a reserved name for this store" % name)
        self.DICT[name] = value

    def _clean(self):
        self.DICT.sync()
        self.DICT.close()


# Ainsi on peut faire store.foo = 'bar' et récupérer store.foo à la session
# suivante. Moi je store tout dans un truc temporaire mais si vous voulez
# garder la persistance entre deux reboots, il suffit de choisir un autre
# dossier.
python_version = "py%s" % sys.version_info.major
try:
    store = Store(os.path.join(TEMP_DIR, 'store.%s.db') % python_version)
except Exception:
    pass
