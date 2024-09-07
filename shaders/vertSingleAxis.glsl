#version 330 core

layout (location = 0) in float pos;
uniform mat4 u_transform;
uniform vec3 unit;

void main()
{
    gl_Position = u_transform * vec4(pos*unit, 1);
}