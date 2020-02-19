# Getting started with git

Git is a source code management tool that allows for distributed teams to work safely and effectively building both highly complex systems and simple systems.

## Basics of Source Code Management

Source code management is a way of saying versioning files and making sure we know what changed, how it changed and why it changed.  We communicate changes through `commits`, `branches`, `merges` and `pull requests`

- commits are a saved point in time
- branches are ways of segregating our work from someone elses work
- merges are how you add another person's changes into your branch
- pull requests are how you ask for others to review your work before adding it to their branch or the root branch.

There are two primary types of source code management; centralized and distributed.  There are advantages and disadvantages to each, but today we're going to ask you to use a distributed system called `git`

### Centralized vs Decentralized 

[What is centralized vs Decentralized SCM?](https://medium.com/faun/centralized-vs-distributed-version-control-systems-a135091299f0)

## Basics of Git

[Git SCM](https://git-scm.com/about)

### Background

Hi Everyone At Drake!

Git has become the defacto standard in source code management.  There are many centralized scm systems which have been used, but git has proven to be faster, safer, and more effective at managing complex distributed collaborative projects.  It was created as a way for open source contributors to the linux kernel to effectively manage changes and bring stability to the platform.

### The Basics of Setting up your Code

[git terms explained](https://linuxacademy.com/blog/linux/git-terms-explained/)

1. You will need to [download git](https://git-scm.com/downloads) onto your machine.
1. Install git from the prior steps download
1. Also [download a github client](https://desktop.github.com/) and install it
1. Create a login for github
1. Create a project (Don't add a README or LICENSE file if you already have existing code)
1. Find your source of truth currently
1. Open a Git Bash window in that folder (right click in the folder)
1. Initialize a local repository using the walk through in your previously created github project

### Using Git

Here are some common scenarios you'll likely encounter with managing your code:

#### I don't have any code for my project on my machine

- [Using the desktop client](https://help.github.com/en/desktop/contributing-to-projects/cloning-and-forking-repositories-from-github-desktop)
- [Using the command line](https://help.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository)

#### I have my code, and I want to make changes safely

- [Creating a branch via desktop client](https://help.github.com/en/desktop/contributing-to-projects/creating-a-branch-for-your-work)
- [Creating a branch via command line](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging)

#### I have made changes to my code and I want to save those changes for myself

- [Committing to a branch via desktop client](https://help.github.com/en/desktop/contributing-to-projects/committing-and-reviewing-changes-to-your-project)
- [Committing to a branch via command line](https://git-scm.com/docs/git-commit)

#### I have saved my changes for myself, and I want to share all of my changes with my team

- [Pushing a branch to github via the desktop client](https://help.github.com/en/desktop/contributing-to-projects/syncing-your-branch)
- [Pushing a branch to github via the command line](https://help.github.com/en/github/using-git/pushing-commits-to-a-remote-repository)

#### I have a teammate who pushed code to github, and I want it!

- [Pulling changes from github via the command line](https://help.github.com/en/github/using-git/getting-changes-from-a-remote-repository)
- [Pulling changes from github via the desktop client](https://help.github.com/en/desktop/contributing-to-projects/syncing-your-branch)

#### We want my code to join our working copy

- [Creating a pull request in github](https://help.github.com/en/desktop/contributing-to-projects/creating-a-pull-request)

#### What does it look like to do this right?

```bash
➜  drake-data-science git:(master) ✗ git clone https://github.com/TractorZoom/drake-data-science.git
Cloning into 'drake-data-science'...
remote: Enumerating objects: 13, done.
remote: Counting objects: 100% (13/13), done.
remote: Compressing objects: 100% (10/10), done.
remote: Total 13 (delta 4), reused 10 (delta 2), pack-reused 0
Unpacking objects: 100% (13/13), done.
➜  drake-data-science git:(master) git checkout -b "updating-workflow"
Switched to a new branch 'updating-workflow'
➜  drake-data-science git:(updating-workflow) git add .
➜  drake-data-science git:(updating-workflow) ✗ git commit -m "adding placeholder for workflow"
[updating-workflow a5d570f] adding placeholder for workflow
 1 file changed, 2 insertions(+)
➜  drake-data-science git:(updating-workflow) git push
fatal: The current branch updating-workflow has no upstream branch.
To push the current branch and set the remote as upstream, use

    git push --set-upstream origin updating-workflow

➜  drake-data-science git:(updating-workflow) git push --set-upstream origin updating-workflow
Enumerating objects: 5, done.
Counting objects: 100% (5/5), done.
Delta compression using up to 12 threads
Compressing objects: 100% (3/3), done.
Writing objects: 100% (3/3), 354 bytes | 354.00 KiB/s, done.
Total 3 (delta 2), reused 0 (delta 0)
remote: Resolving deltas: 100% (2/2), completed with 2 local objects.
remote: 
remote: Create a pull request for 'updating-workflow' on GitHub by visiting:
remote:      https://github.com/TractorZoom/drake-data-science/pull/new/updating-workflow
remote: 
To https://github.com/TractorZoom/drake-data-science.git
 * [new branch]      updating-workflow -> updating-workflow
Branch 'updating-workflow' set up to track remote branch 'updating-workflow' from 'origin'.
➜  drake-data-science git:(updating-workflow) git checkout master
Switched to branch 'master'
Your branch is up to date with 'origin/master'.
➜  drake-data-science git:(master) git pull
remote: Enumerating objects: 1, done.
remote: Counting objects: 100% (1/1), done.
remote: Total 1 (delta 0), reused 0 (delta 0), pack-reused 0
Unpacking objects: 100% (1/1), done.
From https://github.com/TractorZoom/drake-data-science
   4525169..1304153  master     -> origin/master
Updating 4525169..1304153
Fast-forward
 getting-started-with-git.md | 2 ++
 1 file changed, 2 insertions(+)
```

#### I am a part of an organization and I want to allow outside collaborators on a private repository

1. Navigate to your Organization in Github
1. Click `New` to create a new repository
1. Name the repository and indicate it as private, click `create`
1. You should now see the newly created repository, click `settings`
1. Click `Manage access`
1. In the Manage access box click invite teams or people
1. Invite the collaborators you want as a part of that repository

### Other resources

- [git for dummies](https://dev.to/doylecodes/git-for-dummies-1a2i)
- [github help](https://help.github.com/en)
