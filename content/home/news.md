---
# Block & Layout Type
widget: collection
headless: true

# Position on homepage (Lower number = higher on the page)
weight: 20

# Section Title & Subtitle
title: News & Updates
subtitle: Recent public scholarship, media coverage, and announcements.

# Content Rules
content:
  page_type: post
  # Number of items to display on the homepage
  count: 5
  # Filter options (leave blank to show all posts)
  filters:
    author: ''
    category: ''
    tag: ''
    exclude_featured: false
  # Optional button at the bottom pointing to a full archive page
  archive:
    enable: true
    link: 'post/'
    text: 'See all news →'

# Visual Design Settings
design:
  # Layout columns: '1' or '2'
  columns: '2'
  
  # Item Display Style Options:
  # 1 = Standard List | 2 = Compact List | 3 = Grid Cards
  view: 2
  
  # Background Style Configuration
  background:
    color: ''       # e.g., '#f7f7f7' for a light grey accent section
    text_color_light: false
---
