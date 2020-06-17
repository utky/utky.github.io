+++
title = "posts"
+++

<ul>
  {% for page in subsections | sort(attribute="date") | reverse %}
  <li><a href="{{ page.permalink | safe }}">{{ page.title }}</a></li>
  {% endfor %}
</ul>