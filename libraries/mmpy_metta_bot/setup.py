from pathlib import Path

from setuptools import find_packages, setup

excludes = (
    "*test*",
    "*local_settings*",
)


def requires(filename: str):
    return open(filename).read().splitlines()


setup(
    name="mmpy_metta_bot",
    # Updated by publish workflow
    version="1.0.1", #Path(__file__).parent.joinpath("version.txt").read_text().rstrip(),
    author="Team MeTTaLog",
    author_email="logicmoo@gmail.com",
    license="MIT",
    description="A python based bot for Mattermost with its own webhook server.",
    keywords="chat bot metta mattermost",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    url="https://github.com/logicmoo/mmpy_metta_bot",
    python_requires=">=3.8",
    platforms=["Any"],
    packages=find_packages(exclude=excludes),
    install_requires=requires("requirements.txt"),
    extras_require={"dev": requires("dev-requirements.txt")},
    package_data={"mmpy_metta_bot": ["./version.txt"]},
    include_package_data=True,
    entry_points={
        "console_scripts": [
           #  "mmpy_metta_bot = mmpy_metta_bot.cli:main",
        ],
    },
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "License :: OSI Approved :: MIT License",
        "Intended Audience :: Developers",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Topic :: Internet :: WWW/HTTP",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
)
