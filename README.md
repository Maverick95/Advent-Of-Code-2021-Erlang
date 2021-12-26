As I make further progress with this, I should organise this markdown into something that is more coherent and less just me randomly smashing my thoughts into a document. But currently, that's what this is, so enjoy.

I just wanted to pick a new language to try some Advent of Code stuff with, sounds weird to say but after my first year as a proper software engineer I didn't feel like picking some "relevant", I felt like doing something completely separated from work-related stuff.

Erlang seemed like a weird one, a lot of positive comments about it on the internet but quite a low usage base. A few Google searches and you figure out why, it's a language privately maintained and copyrighted to Ericsson and was made in the 80s.

So far Erlang is very different to anything I've used before. As such building even simple programs is a complete head f**k at first.

I'm still not quite sure of the differences between a server, a state machine, and an event manager / handler, apart from a couple of odd characteristics they all seem pretty similar in function and form.

The help documents are excellently laid out, none of the effort involved regards navigating them, all the effort so far is in reversing normally common operations in your brain to fit Erlang's desired model.

It is a Microservices language at its core, so far I haven't had much time to get to grips with all the stuff like releasing / replacing code and services, I'm still at the beginners part.

The features I like are the destructuring, its interesting to see this in a language 20 years old being adopted by Javascript in the last ES, I wonder if somebody stumbled across Erlang and went "hey this is good let's throw it in".
But I guess like in Javascript especially with React which is functional you're constantly passing prototypes around and you need to access elements quickly.

The root of Erlang's strength is in the way functions and case statements are defined, you write multiple function definitions based on the types / conditions of arguments received. This means you can build a function that handles 3 different object types in no time at all, compared to say -
Javascript, where you'd have to messily destructure and check arguments against a big wall of ifs and cases.
C#, where you'd have to battle against the strong types.
Erlang is more about just letting your data be data, wonder if this means I'll run into the same problems as with vanilla JS, where unfolding your own errors becomes an exercise in frustration, especially when the language is non-boilerplate and minimises keystrokes.
I don't want to have to decode 20 lines of symbols [ with [ code | written] ++ [like, this]] to identify a misplaced bracket.

Interesting in how Erlang works under the hood. As if I'd understand it. Can imagine it being a nightmare to optimise.

Syntactically it's pretty simple, not many different commands to learn, minimal typing to maximal output, its in no way as verbose as something like C# or even JS. You just use fewer words generally.

As a language on it's own, bashing out Advent-of-Code scripts wouldn't have been much fun. It's been more of a challenge to write generic solutions using Erlang's OTP structure, in my case, each "Part" is expressed as a server that you throw the individual lines of the puzzle input to. Servers have individual "state" which is affected by these.

One thing that's not very clear is to do with passing by reference or value, for example if you have a complex state variable in a server, do you need to overwrite it each time or alter a reference, probably the former.

Main problem is it doesn't work like any language I've encountered, so every problem seems like a headache. I'm trying to implement Day 5 at the moment (well I'm about to start).

The VS Code helper for Erlang can be a bit flaky, sometimes if you get a syntax error highlighted in red, if you delete/add a character to leave the same code the error will disappear.

Erlang encourages you to write fewer lines / characters of code. It is very "whatever the opposite of verbose is". Concise. That's the word. Concise.

Disadvantages - sometimes you get multiple nested case-s and ifs where the else returns say an error. In these cases EVERY NESTED CONDITIONAL STATEMENT must define the "else" condition, or else (no pun intended) you get a run-time error.

Also no offence but WHY in Erlang are the inequality operators COMPLETELY DIFFERENT THAN IN EVERY OTHER LANGUAGE KNOWN TO HUMANKIND.

Config setup can be confusing, error logs are not always clear.
