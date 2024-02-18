from setuptools import setup, find_packages
import io
import sys
import glob


with open("README.md") as f:
    readme = f.read()

setup(
	  name="vspace_metta",
      version="0.1.1",
      author="Douglas R. Miles",
      author_email="logicmoo@gmail.com",
      description="A MeTTa kernel for Jupyter that can use Python libraries",
      long_description=readme,
      long_description_content_type="text/markdown",
      url="https://github.com/logicmoo/hyperon-wam",
      
      packages=find_packages(include=["src", "*"]),
      package_data={"src": ["images/*.png", "modules/*.ss"]},
      platforms=["Any"],
      scripts = ["scripts/metta-jupyter-kernel", "scripts/metta-jupyter-kernel-debug"],
      data_files=[
          ("share/jupyter/kernels/src",
           ["src/kernel.json"] + glob.glob("src/images/*.png")
          )
      ],
      install_requires=["metakernel", "yasi"],
      classifiers = [
          "Framework :: IPython",
          "License :: OSI Approved :: BSD License",
          "Programming Language :: Python :: 3",
          "Programming Language :: Python :: 2",
          "Programming Language :: MeTTa",
          "Topic :: System :: Shells",
      ]
)
