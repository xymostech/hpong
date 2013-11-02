#version 140

in vec4 position;
uniform vec2 offset;

void main() {
     gl_Position = position + vec4(offset.x, offset.y, 0, 0);
}