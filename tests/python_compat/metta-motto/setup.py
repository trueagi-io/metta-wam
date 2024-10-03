from setuptools import setup, find_packages

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
       name="metta-motto",
       version="0.0.7",  # Update as appropriate
       description="Integration of MeTTa and LLMs for prompt templates, guidance, and chaining as well as composition with other agents.",
       long_description=long_description,
       long_description_content_type="text/markdown",
       url="https://github.com/zarqa-ai/metta-motto",
       packages=find_packages(),
       classifiers=[
        'Development Status :: 3 - Alpha',  # Development status
        'Intended Audience :: Developers',  # Intended audience
        'License :: OSI Approved :: MIT License',  # License
        'Programming Language :: Python :: 3.8',  # Python versions supported
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10',
        'Programming Language :: Python :: 3.11',
        'Operating System :: POSIX :: Linux',
        'Operating System :: MacOS :: MacOS X'
       ],
       python_requires='>=3.8',
       install_requires=[
        # List your project's dependencies here.
        'hyperon',
        #'mettalog',
        'openai'
        ]
   )
