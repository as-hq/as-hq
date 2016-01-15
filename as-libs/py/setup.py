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
      install_requires=['numpy','matplotlib','pandas','wget','statistics','ipython','zmq','colour'],
      zip_safe=False)