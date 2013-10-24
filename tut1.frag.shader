#version 140

out vec4 outputColor;

void main() {
     outputColor = vec4(gl_FragCoord.x / 800.0, 1.0, 1.0, 1.0);
}
