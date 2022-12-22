# zweidoom

> My Doom Emacs configuration. [See here for my other dotfiles](https://github.com/Zweihander-Main/zweidotfiles)

## External packages:

I've broken out some parts of this config as external packages:

- **[ZweiGTD-Goals](https://github.com/Zweihander-Main/zweigtd-goals)** - Track a user-defined set of goals and priorities
- **[ZweiGTD-Reviews](https://github.com/Zweihander-Main/zweigtd-reviews)** - Create time-span reviews for goals
- **[Process-Org-Agenda-Inbox](https://github.com/Zweihander-Main/process-org-agenda-inbox)** - Process GTD inbox agenda buffer quickly
- **[Org-Statistics-Cookie-Helpers](https://github.com/Zweihander-Main/org-statistics-cookie-helpers)** - Library for working with statistics cookies
- **[Org-Agenda-Heading-Functions](https://github.com/Zweihander-Main/org-agenda-heading-functions)** - Library for working with org-agenda headings
- **[Kindle-Highlights-To-Org](https://github.com/Zweihander-Main/kindle-highlights-to-org)** - Take a Kindle notes file and convert it to an org tree

## Notes:

- Packages are managed using Doom's package manager.
- [init-loader](https://github.com/emacs-jp/init-loader) is used extensively for loading config in `/lisp` folder
- Cask is used purely for development of the config -- test running, linting, ect.
- Elsa is available but doesn't do well in private configs/with some libraries so is there just for linting (see the Makefile)

## Available for Hire

I'm available for freelance, contracts, and consulting both remotely and in the Hudson Valley, NY (USA) area. [Some more about me](https://www.zweisolutions.com/about.html) and [what I can do for you](https://www.zweisolutions.com/services.html).

Feel free to drop me a message at:

```
hi [a+] zweisolutions {●} com
```

## License

[AGPLv3](./LICENSE)

    zweidoom
    Copyright (C) 2021 Zweihänder

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
