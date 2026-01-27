I guess this there is an idea solidifying in my had.

# Design Philosophy

Here are some guidelines for my self to remind me what is nice to remember.

## Font size

The font size is always in `rem` unless really necessary. Which is probably never.

## Margin and padding

In `em` since it simplifies the handling of things.

## CSS classes

Each class should ideally be on small thing and be used to do one thing only.
Something like:

```css
.border {
  border-width: 0.1em;
  border-style: solid;
  border-color: var(--text-secondary-color);
  box-shadow: hsla(129, 87%, 62%, 0.6) 0 0 0, inset hsla(129, 87%, 62%, 0.6) 0 0 0.5em;
}
```

Going down into the depths of utility classes is a bit of because it is heavy
on tooling.

Sadly there is not good way to add selectors to classes so they need to
sometimes exists clones of the same class. Like:

```css
.border-4-hover:hover:not([disabled]) {
}
.border-4-focus:focus:not([disabled]) {
}
```

And we shell see where this all leads to ;).
