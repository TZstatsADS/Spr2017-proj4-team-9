#!/usr/bin/env python
import os
import codecs

rexa_dir = './rexa/'
field_names = [
    'author-in-focus', 'author-in-focus-score',
    'authorlist', 'alt-authorlist', 'altTitle', 
    'editor', 'email', 'institution', 'journal',
    'abstract', 'body', 'keyword', 'title', 'year'
]
authors_f = []

for root, dirs, files in os.walk('./rexa', topdown=False):
    for f in files:
        if f.endswith('.txt'):
            authors_f.append({
                'path': '{}/{}'.format(root, f),
                'cluster_name': root.split('/')[-1]
            })
print(authors_f[0])

def raw2dict(raw):
    return { entry.split(':', 1)[0].strip():entry.split(':', 1)[1].strip() for entry in raw if entry.strip() != ''}

from xml.etree import ElementTree as ET

def clean_name(xml_string):
    root = ET.fromstring(xml_string)
    return ' '.join([child.text for child in root])

def fill_na(author):
    for field in field_names:
        if author.get(field, None) is None:
            author[field] = None
    return author

authors_d = []

for author_f in authors_f:
    with codecs.open(author_f['path'], 'r', 'utf-8', errors='ignore') as f:
        author_raw = raw2dict(f.readlines())
        author_raw['cluster_name'] = author_f['cluster_name']
        authors_d.append(author_raw)
print(len(authors_d))

# Clean author data
authors_dict = {}

for index, author_d in enumerate(authors_d):
    fill_na(author_d)
    author_d['author-in-focus'] = clean_name(author_d['author-in-focus'])
    author_d['authorlist'] = tuple([clean_name(author_name) for author_name in author_d['authorlist'].split('%%')])
    if author_d.get('alt-authorlist', None) is not None:
        author_d['alt-authorlist'] = tuple(clean_name(author_name) for author_name in author_d['alt-authorlist'].split('%%'))
    if author_d.get('keyword', None) is not None:
        author_d['keyword'] = tuple([keyword.strip() for keyword in author_d['keyword'].split(',') ])
    if author_d.get('author-in-focus-score', None) is not None:
        author_d['author-in-focus-score'] = float(author_d['author-in-focus-score'])
    authors_dict[index] = author_d

from itertools import groupby

training_data = dict([(key, tuple(group)) for key, group in groupby(authors_d, lambda item: item['cluster_name'])])

import dedupe

fields = [
    {'field' : 'author-in-focus', 'type': 'String'},
    {'field' : 'author-in-focus-score', 'type': 'Price'},
    {'field' : 'authorlist', 'type': 'Set'},
    {'field' : 'alt-authorlist', 'type': 'Set', 'has missing' : True},
    {'field' : 'email', 'type': 'String', 'has missing' : True},
    {'field' : 'keyword', 'type': 'Set', 'has missing' : True},
    {'field' : 'abstract', 'type': 'Text', 'has missing' : True},
    {'field' : 'body', 'type': 'Text', 'has missing' : True},
    {'field' : 'journal', 'type': 'String', 'has missing' : True},
    {'field' : 'institution', 'type': 'String', 'has missing' : True},
]

deduper = dedupe.Dedupe(fields)

deduper.sample(authors_dict)

match_clusters = [
    'SarahJRussell',
    'DMAllen-ohu',
    'AlvinBlum',
    'SJonesKnowEng',
    'MAJordan',
    'DKoller',
    'LHLee-elec',
    'JGMcGuire',
    'AlanMoore',
    'RajeevMotwani',
    'SebastianThrun',
    'StephenJYoung'
]

distinct_clusters = [
    ('SAYoung', 'SCKYoung'),
    ('SAYoung', 'SCKYoung'),
    ('StephenRussell', 'StephenRussellBIO'),
    ('RajeevMotwani', 'RaviMotwani'),
    ('AndrewJMoore', 'AndrewMMoore'),
    ('JBMcGuire', 'JGMcGuire'),
    ('LALee1', 'LALee2'),
    ('DKoller', 'DanielKoller'),
    ('MarilynJordan', 'MauriceJordan'),
    ('SCJones1', 'SCJones2'),
    ('AlvinBlum', 'AvrimBlum'),
    ('DAllen-jr', 'DAllen-ucla'),
]

deduper.markPairs({
    'match': [
        (training_data.get(cluster_name)[-1], training_data.get(cluster_name)[-2])
        for cluster_name in match_clusters
    ],
    'distinct': [
        (training_data.get(cluster_l)[0], training_data.get(cluster_r)[0])
        for cluster_l, cluster_r in distinct_clusters
    ]
})

print('start training...')

deduper.train()

print('finished...')

training_file = 'author_training.json'

with open(training_file, 'w') as tf:
    deduper.writeTraining(tf)
    
threshold = deduper.threshold(authors_dict, recall_weight=1)

print('clustering...')
clustered_dupes = deduper.match(authors_dict, threshold)

print('# duplicate sets', len(clustered_dupes))
