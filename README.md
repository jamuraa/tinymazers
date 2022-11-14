# Tiny Maze Bot

A fediverse bot that makes tiny mazes and posts them to a Mastodon server.

Migrated from the [twitter bot](https://github.com/jamuraa/tinymazes) that does the same.

## Installation

1. (maybe optional?) Setup an application on your mastodon instance.
   The code defaults to the [botsin.space](https://botsin.space) instance.
   Use the `urn:ietf:wg:oauth:2.0:oob` redirect URI.
   The application needs the `read` (for future use) and `write:statuses` scopes at a minimum.
1. `cargo run`

   1. (first time setup) It will prompt with a url to authenticate your account.
      Copy-paste the token from the resulting page into the cli prompts.
   1. A maze will be posted to the timeline.

There is no automatic timing, schedule the bot to run using `cron` or a shell loop, or whatever.

## Currently running

The tinymazers bot is currently running every half hour at
[@tinymazes@botsin.space](https://botsin.space/@tinymazes).
