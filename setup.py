import setuptools

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()
    
setuptools.setup(
    name = 'parsesql',
    version = '0.1',
    author = 'Wade Lieurance',
    author_email = 'wlieurance@gmail.com',
    description = 'A simple class based SQL parser that separates text SQL statements into vectors, formats them with parameters, determines character states, and creates a color/style formatted output for printing.',
    long_description = long_description,
    long_description_content_type = 'text/markdown',
    url = 'https://github.com/wlieurance/parsesql',
    project_urls = {'Bug Tracker': 'https://github.com/wlieurance/parsesql/issues'},
    classifiers = [
    'Development Status :: 2 - Pre-Alpha',
    'Programming Language :: Python :: 3',
    'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
    'Operating System :: OS Independent',
    'Natural Language :: English',
    'Topic :: Text Processing',
    ],
    packages=['parsesql'],
    package_dir={"": "python"},
    python_requires=">=3.6",
    install_requires=['colorama']
)

