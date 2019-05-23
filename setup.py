from distutils.core import setup

setup(
    name='powly-parsers',
    version='0.0.1',
    packages=['powly.parsers'],
    url='https://github.com/patrickwestphal/powly',
    license='GPLv3',
    author='Patrick Westphal',
    author_email='patrick.westphal@informatik.uni-leipzig.de',
    description='',
    install_requires=[
        'powly',
        'pyparsing>=2.1.9'
    ]
)
