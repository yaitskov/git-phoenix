# git-phoenix

This is a command line tool for recovery Git repositories after
accidental removal or file system failure.

## Motivation

The tool is started as a practical attempt to retrieve a few
unpublished repositories from an EXT4 disk, with hundreds of other
repos including very big ones (nixpkgs, Linux kernel, GHC), played a
role of haystack here.

EXT4 has tools, such as extundelete and ext4magic, but they didn't
succeed in that case.  So I switched to more primitive tool
(photorec), producing independent files with arbitrary names, based on
magic headers and other content heuristics, specific to particular
format, and luck that files take sequencial disk blocks. The tool did
the job, but requried magic tuning, because by default GIT object
files are skipped.  Rescued git files usually were bigger than
compressed content and had trailing trash bytes.

## Getting input with photorec

git-phoenix need input produced by photorec or similar tool.  To make
photorec recognize the zlib file format put following config into
`~/.photorec.sig` on livecd. I used system-rescue distributive.

```
go1 0 0x7801
go2 0 0x78DA
go3 0 0x789C
```

Launch photorec without arguments - it has ncurses terminal UI.

photorec output looks like:

```shell
$ tree /paranoid-no-brutforce-nonexpert-nocorrupted-zlib/
|-- recup_dir.1
|   |-- f0305926.go1
|   |-- f0378540.go1
|   |-- f0421825.go1
...
|-- recup_dir.1055
    ...
    |-- f167043017.go1
    `-- f167043025.go1
$
```

## git-phoenix recovery steps

git-phoenix sunny day scenario assumes execution of several commands to
get GIT repo from photorec output.

### Step 1. Building uber repo

Uber repo is a folder with structure equal to `.git/objects` but
instead of regular files symlinks point to files in photorec
structure.

```shell
$ git-phoenix uber -o uber -i /paranoid-no-brutforce-nonexpert-nocorrupted-zlib/
Duration: 45.71s
Found:    423254
Speed:    9260.03 files per second
Maximum number of SHA collisions: 17
$
```

Uber command picks valid GIT objects and mitigates SHA collisions,
which is pretty common in this situation.

### Step 2. Discovery HEAD commit

This step is optional if you managed to recover reflog by simply
grepping commit comment just pick the latest hash from there.

Command prints SHAs of consistent commit chains i.e. ending with a
commit without parent.

```shell
$ git-phoenix extract -g my-foo -u uber -s 8fb567617e7
7768eed9387ff 2025.12.31 23:45 John Dow - tests fixed
```

### Step 3. Real GIT repo extraction

Uber repo should contain all required files, but it is not a valid GIT
repo.  GIT thoroughly checks object files and complains about any
trailing trash.  Extraction creates GIT repo with master branch
referring specified commit, chopping off trailing trash and
disambiguating SHAs.

```shell
$ git-phoenix extract -g my-foo -u uber -s 8fb567617e7
$ cd my-foo
$ git checkout .
$ echo Woo-Hah
```
