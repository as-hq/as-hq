from setuptools import setup, find_packages

setup(name='AS',
      version='0.1.3',
      description='AlphaSheets Proprietary Libraries (tm)',
      url='http://alphasheets.com',
      author='AlphaSheets',
      author_email='team@alphasheets.com',
      license='',
      packages=find_packages(),
      package_data={'AS': ['data/*.txt']},
      install_requires=['traitlets', \
                        'numpy', \
                        'matplotlib', \
                        'pandas', \
                        'wget', \
                        'statistics', \
                        'scipy', \
                        'ipython', \
                        'zmq', \
                        'colour', \
                        'pysqldf', \
                        'openpyxl', \
                        'shortid', \
                        'beautifulsoup4'
                        ],
      zip_safe=False)
