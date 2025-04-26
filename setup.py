from setuptools import setup, find_packages

setup(
    name='PACGBI-TOOL',
    version='0.1.0',
    packages=find_packages(),  # Will find 'tool' automatically
    include_package_data=True,
    install_requires=[
        'click==8.1.8',
        'dotenv==0.9.9',
        'mistralai==1.7.0',
        'networkx==3.4.2',
        'numpy==2.2.5',
        'PyGithub==2.6.1',
        'python-dotenv==1.1.0',
        'scikit-learn==1.6.1',
        'scipy==1.15.2',
    ],

    entry_points={
        'console_scripts': [
            'pacgbi=Tool.pipeline:main',  # tool is folder, pipeline is the file
        ],
    },
    author='Surya, Dhyanam, Adithya, Anirudh, Sudhanva, Srikar',
    author_email='cs23b016@iittp.ac.in, cs23b014@iittp.ac.in, cs23b001@iittp.ac.in, cs23b008@iittp.ac.in, cs23b051@iittp.ac.in, cs23b049@iittp.ac.in',

    description='PACGBI-TOOL: A CLI tool to analyze and fix COBOL functions from GitHub issues.',
    long_description=open("README.md").read(),  
    long_description_content_type="text/markdown",
    url='https://github.com/AnirudhArrepu/PACGBI-Tool-Dev',
    classifiers=[
        "Programming Language :: Python :: 3",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
