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
      install_requires=['traitlets==4.0.0', \
                        'numpy', \
                        'matplotlib', \
                        'pandas', \
                        'wget', \
                        'statistics', \
                        'scipy', \
                        'ipython==4.0.0', \
                        'zmq', \
                        'colour', \
                        'pandasql', \
                        'openpyxl', \
                        'shortid'
                        ],
      zip_safe=False)